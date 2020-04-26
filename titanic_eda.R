library(dplyr)
library(ggplot2)
library(tidyverse)

library(mice)
library(RColorBrewer)
library(scales)
library(ggpubr)


train <- read_csv('train.csv')
test <- read_csv('test.csv') #test has 1 less col 
test$Survived <- NA
original <- rbind(train,test)
titanic <- original
attach(titanic)

#High-level stats 
names(train)
summary(titanic) #we see some missing values 
glimpse(titanic$Name)


#PCLASS 

titanic %>% 
  count(Pclass) %>% 
  mutate(prop=round(n/sum(n)*100)) 

#NAME --> Title
titanic$Title <- gsub('(.*, )|(\\..*)', '', titanic$Name)
table(titanic$Sex, titanic$Title)
count(titanic, Title) %>% arrange(desc(n))
titanic <- titanic %>% 
  mutate(Title = case_when(Title %in% c('Master', 'Miss', 'Mr', 'Mrs') ~ Title,
                           Title %in% c('Mlle', 'Mme', 'Ms') ~ "Miss",
                           TRUE ~ "Honored"))
titanic$Title <- as.factor(titanic$Title)
titanic$Name <- NULL

#SEX 
titanic %>% 
  count(Sex) %>% 
  mutate(prop=round(n/sum(n)*100)) %>% 
  arrange(desc(prop))

#and SibSp & Parch contain similar info - we can combine
#Create: Family 
titanic$Family = SibSp+Parch+1
titanic$SibSp <- NULL 
titanic$Parch <- NULL

titanic <- titanic %>% 
  mutate(Family_Size=case_when(Family == 1 ~ "0",
                               Family >1 & Family<5 ~ "1",
                               Family>=5 ~ "2"))
titanic$Family_Size <- as.factor(titanic$Family_Size)
#titanic$Family <- NULL

#all missing values  
titanic %>% 
  summarize_all(funs(sum(is.na(.))))
#to-do: deal with Fare, Cabin, Embarked, Age N/As 


#there are some values that should be Factors 
factors <- c('PassengerId', 'Survived','Pclass', 'Ticket', 'Sex','Embarked',
             'Cabin')
titanic[factors] <- lapply(titanic[factors], function(x) as.factor(x))
titanic$Survived <-factor(titanic$Survived, levels =c(0,1), labels =c('No','Yes'))
#titanic$Survived <-factor(titanic$Survived, levels =c('No','Yes'), labels =c('0','1'))


#unique column variables
lapply(titanic, function(x)length(unique(x)))
#insight: there are 929 unique tickets and 1309 passengers 
#there are passengers on the same ticket; the Fare is for the ticket, so we will 
#need to calculate this 
#to-do: create Fare_Per_Person (we need Fare, and number of people per ticket)

#_____________________________________________
#FARE 

titanic %>% 
  ggplot(aes(y=Pclass, x=Fare))+
  geom_boxplot()

titanic %>% 
  group_by(Pclass) %>% 
  summarize(median_Fare = round(median(Fare, na.rm=TRUE)))

#what does Fare really mean? 
#proof that Fare is per ticket, and per ticket can have multiple people 
#we can see clear trend that within the same class, Fare increase as #people per ticket increases
titanic %>% 
  filter(Pclass==2) %>% 
  count(Ticket, Fare) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=n, y=Fare)) + 
  geom_point()
#therefore to make it comparable - we need Fare Per Person 
#Create: Fare_Per_Person
n_per_ticket <- titanic %>% 
  group_by(Ticket) %>% count() %>% 
  arrange(desc(n))
titanic <- merge(x=n_per_ticket, y=titanic, 
                 by.x="Ticket", by.y="Ticket",
                 all.x=TRUE, all.y=TRUE)
colnames(titanic)[colnames(titanic) == "n"] <- "N_Per_Ticket"
titanic$Fare_Per_Person <-  titanic$Fare/titanic$N_Per_Ticket
# titanic$N_Per_Ticket <- NULL
# titanic$Ticket <- NULL

head(titanic)
titanic <- titanic %>% arrange(PassengerId)

titanic %>% 
  group_by(Pclass) %>% 
  summarize(median_Fare = round(median(Fare, na.rm=TRUE)),
            median_FarePP = round(median(Fare_Per_Person, na.rm=TRUE)))

titanic %>% 
  filter(Pclass==3) %>% 
  ggplot(aes(x=N_Per_Ticket, y=Fare_Per_Person)) + 
  geom_point()

###1 Missing value of Fare (and Fare_Per_Pereson)
filter(titanic, is.na(Fare))
filter(titanic, Ticket=='3701') #see if anyone else has same ticket - no
#this person is traveling alone, we also see he is in Class3 
#let's fill this N/A with median value of Fare from Class 3
temp_fare <- titanic %>% 
  filter(Pclass==3) %>% 
  summarize(median(Fare_Per_Person, na.rm = T))
titanic$Fare_Per_Person[is.na(titanic$Fare_Per_Person)] <- as.numeric(temp_fare)


#we see there are still a lot of outliers 
titanic %>% 
  ggplot(aes(x=Fare_Per_Person, y=Pclass))+
  geom_boxplot() 

titanic %>% 
  filter(Pclass==3) %>% 
  ggplot(aes(Fare_Per_Person))+
  geom_histogram()

#check for outliers in Fare_Per_Person 

q1 <- quantile(filter(titanic, Pclass==1)$Fare_Per_Person)
q2 <- quantile(filter(titanic, Pclass==2)$Fare_Per_Person)
q3 <- quantile(filter(titanic, Pclass==3)$Fare_Per_Person)

q1
q2
q3
q1[1]
first <- filter(titanic, Pclass==1)$Fare_Per_Person
second <- filter(titanic, Pclass==2)$Fare_Per_Person
third <- filter(titanic, Pclass==3)$Fare_Per_Person
out1 <-  boxplot(first, plot=FALSE)$out
out2 <-  boxplot(second, plot=FALSE)$out
out3 <-  boxplot(third, plot=FALSE)$out
out3
out3 <-  third[third>15 | third==0]
cap1 <- c(min(first[!first %in% out1]), max(first[!first %in% out1]))
cap2 <- c(min(second[!second %in% out2]), max(second[!second %in% out2]))
cap3 <- c(min(third[!third %in% out3]), max(third[!third %in% out3]))
cap3
cap2
cap1
cap1[2]
#create Odd_Fare
#capture extreme variables 
#1=too low, 2=too high, 0=not_odd
titanic <- titanic %>% 
  mutate(Free_Fare = case_when(Fare_Per_Person == 0 ~1,
                               TRUE ~0))
table(titanic$Free_Fare)                              
titanic$Free_Fare <- as.factor(titanic$Free_Fare)
#update Fare_Per_Person to caps per class 


titanic %>% filter(Pclass==3) %>% 
  ggplot(aes(x=Fare_Per_Person))+
  geom_boxplot()




cuts <- c(-Inf, 7, 8, 10.5, 25, 38, Inf)
labs <- c("0-7", "7-8", "8-10.5", "10.5-25", "25-38", "38+")
titanic <- titanic %>% 
  mutate(Fare_Groups = cut(Fare_Per_Person, breaks = cuts, labels=labs))
titanic$Fare_Groups <- as.factor(titanic$Fare_Groups)

table(titanic$Fare_Groups)
###Fare+Class 


# titanic <- titanic %>% 
#   mutate(Fare_And_Class = case_when(Pclass==1 & Fare_Per_Person > cap1[2] ~ 1,
#                                     Pclass==1 & between(Fare_Per_Person, cap1[1], cap1[2]) ~ 2,
#                                     Pclass==1 & Fare_Per_Person < cap1[1] ~ 3,
#                                     
#                                     Pclass==2 & Fare_Per_Person > cap2[2] ~ 4,
#                                     Pclass==2 & between(Fare_Per_Person, cap2[1], cap2[2]) ~ 5,
#                                     Pclass==2 & Fare_Per_Person < cap2[1] ~ 6,
#                                     
#                                     Pclass==3 & Fare_Per_Person > cap3[2] ~ 7,
#                                     Pclass==3 & between(Fare_Per_Person, cap3[1], cap3[2]) ~ 8,
#                                     Pclass==3 & Fare_Per_Person < cap3[1] ~ 9))
# 
# titanic$Fare_And_Class <- as.factor(titanic$Fare_And_Class)

#_____________________________________________
###2 Missing value of Embarked
filter(titanic, is.na(Embarked))

#these two share the same Ticket and Cabin - assume they Embarked together 
#given that they are in 1st class, where are they most likely to have Embarked? 
titanic %>% 
  filter(Pclass==1 & !is.na(Embarked)) %>% 
  count(Embarked) %>% 
  mutate(prop=n/sum(n)*100)

#does where you embark influence how much you pay? 
#looking at the boxplots for 1st-class fare and Embarkment 
titanic %>% 
  filter(Pclass==1& (Embarked=='C' | Embarked=='S')) %>% 
  ggplot(aes(x=Embarked, y=Fare))+
  geom_boxplot() +
  ylim(0, 100)
#it seems like those who embarked at 'C' are likely to have paid more, but
#$80 is well within the range of 'S' as well. In addition, the prior probability
#of emarking at 'S' is higher. 

#Since there are only 2 missing values, we will use this approximation 
titanic$Embarked[is.na(titanic$Embarked)] <- 'S'
#check that there are no more NAs 
anyNA(titanic$Embarked)


#_____________________________________________
###1309 Missing value of Cabin (80%)
#1) values are missing b/c these people did not have cabins (this could affect survival)
#2) values are missing b/c we do not know which cabins they had
#this article about cabins on Titanic was informative 
#https://titanic.fandom.com/wiki/Third_Class_cabins
#https://titanic.fandom.com/wiki/First_Class_Staterooms
#"steerage" (open-space dorms) did not apply to Titanic. All passengers
#were housed in cabins; so these data are simply missing 

#Cabin has important information about the Deck the passenger's accomodation is on
#since lower decks were flooded first as the ship sinked, this mattered 

#extract Deck from Cabin -- most follow the format "A10", with the first letter indicating deck
#except 
#1) -- within'F', there are a few funky looking:
titanic %>% 
  filter((!Pclass==1) & !is.na(Cabin) & substr(Cabin, start=1, stop=2)=='F ')
#2) #there is a Deck T - does not exist on the ship; must be mistake. override to "Unknown"

#create a new feature "Deck"
titanic <- titanic %>% 
  mutate(Deck = case_when(is.na(Cabin) ~ "Unknown",
                          substr(Cabin, start=1, stop=1)=="T" ~ "Unknown",
                          substr(Cabin, start=1, stop=2)=='F ' ~ substr(Cabin, start=3, stop=3),
                          TRUE ~  substr(Cabin, start=1, stop=1)
  ))
#see the resulting variable - 
count(titanic, Deck)

#let's drop Cabin 
titanic$Deck <- as.factor(titanic$Deck)
titanic$Cabin <- NULL
titanic$Fare_Per_Person <- NULL
titanic$N_Per_Ticket <- NULL
titanic$Fare <- NULL
titanic$Family <- NULL
titanic$Ticket <- NULL

glimpse(titanic)
#_____________________________________________
###263 Missing value of Age (20%)

#20% of the Age data is missing, can we replace the missing data with median?
sum(is.na(train$Age))/dim(train)[1]
median(titanic$Age, na.rm=TRUE)
#are there any patterns of missing data? 
#maybe some Ports are worse at recording Age data? 
#maybe some there is more missing data in Class

#data could missing if at a certain port, people were not registered 
#Looking at Embarked, if you Embarked at Q, the proprtion of 
#missing age is 59%, compared to only 14% at S 
titanic %>% 
  group_by(Embarked) %>% 
  summarize(n =n(), 
            missing= sum(is.na(Age)), 
            prop_missing = round(missing/n*100),
            med_age = median(Age, na.rm=TRUE)) %>% 
  arrange(desc(prop_missing)) 
#Pclass is also important. 3rd class seems more missing data
titanic %>% 
  group_by(Pclass) %>% 
  summarize(n =n(), 
            missing= sum(is.na(Age)), 
            prop_missing = round(missing/n*100),
            med_age = median(Age, na.rm=TRUE)) %>% 
  arrange(desc(prop_missing))


titanic %>% 
  ggplot(aes(x=Age)) +
  geom_boxplot()




##__________predicting Age 

#title and age --- 
#'Master refers to male aged below 18' -- we can use this info to fill into missing Age
titanic %>% 
  filter(Title=='Master') %>%
  ggplot(aes(Age))+
  geom_boxplot()
master_age <- titanic %>% 
  filter(Title=='Master') %>%
  summarize(mean(Age, na.rm=TRUE))
titanic$Age[is.na(titanic$Age)&titanic$Title=='Master'] <- as.numeric(master_age)

#the median age is 26
#but using this to fill in 20% of the dataset probably is not appropriate
#there is a lot of variation in Age, and it seems like it coule be an important preditor 

names(titanic)

ignore <- c('PassengerId', 'Survived')
keep <- names(titanic[!names(titanic) %in% ignore])

set.seed(42)
mice_mod <- mice(titanic[, names(titanic) %in% keep], method='rf') 
mice_output <- complete(mice_mod)
titanic$New_Age<- mice_output$Age

ggplot(titanic,aes(x=Embarked, y=Age)) +
  geom_boxplot(aes(fill=Pclass))+
  ylim(0,60)
ggplot(titanic,aes(x=Embarked, y=New_Age)) +
  geom_boxplot(aes(fill=Pclass))+
  ylim(0,60)

#Embark
titanic %>% 
  group_by(Embarked) %>% 
  summarize(n =n(), 
            missing= sum(is.na(Age)), 
            prop_missing = round(missing/n*100),
            med_age = median(Age, na.rm=TRUE),
            med_NEW = median(New_Age, na.rm=TRUE)) %>% 
  arrange(desc(prop_missing)) 
#Pclass 
titanic %>% 
  group_by(Pclass) %>% 
  summarize(n =n(), 
            missing= sum(is.na(Age)), 
            prop_missing = round(missing/n*100),
            med_age = median(Age, na.rm=TRUE),
            med_NEW = median(New_Age, na.rm=TRUE)) %>% 
  arrange(desc(prop_missing))

titanic$Age <- titanic$New_Age
titanic$New_Age <- NULL

head(titanic)

#since we made replacement of 20% of the data, maybe using binning will be helpful
#10 groups of ~size
titanic <- titanic %>% 
  mutate(Age_Group = case_when(Age < 16 ~ "0-15",
                               Age >= 16 & Age < 20 ~ "16-19",
                               Age >= 20 & Age < 24 ~ "20-23",
                               Age >= 24 & Age < 30 ~ "24-29",
                               Age >= 30 & Age < 40 ~ "30-39",
                               Age >= 40 & Age < 55 ~ "40-54",
                               Age >= 55 ~ "55+"))
titanic %>% 
  count(Age_Group)
titanic$Age_Group <- as.factor(titanic$Age_Group)
titanic$Age <- NULL
titanic %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x = Age_Group, fill=Survived))+
  geom_bar(position='dodge')


names(titanic)
titanic <- titanic[c("PassengerId","Survived","Pclass","Title","Sex", "Age_Group", 
                     "Family_Size", "Fare_Groups","Embarked","Deck")]  
glimpse(titanic)

ttrain <- titanic[1:891,]
anyNA(ttrain$Survived)
ttest <- titanic[892:1309,]
write.csv(ttrain,"preprocessed_train.csv", row.names = FALSE)
write.csv(ttest,"preprocessed_test.csv", row.names = FALSE)





#________________EDA______________________________________________________________________________________

#store 
data <- titanic
#Survived Pclass Sex Age SibSp Parch Fare 
#Embarked  Title  Family Deck Age_Group

####Univariate 
#what if you paid very little (0 possible) or a lot (luxury)
#####-----Title -->
#not surprising as age and gender told us similar story 
#one small thing - with an "Honored" title, survival not more likely 

data %>% 
  filter(!is.na(Survived) & Pclass!=3 & Fare_Per_Person<30) %>% 
  ggplot(aes(x=cut_width(Fare_Per_Person, width=10), fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+ theme(legend.position="none")+
  facet_wrap(~Pclass)

sclass <- data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Pclass, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+ theme(legend.position="none")


sage <- data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Age_Group, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+ theme(legend.position="none")

sdeck <- data %>% 
  filter(!is.na(Survived) & Deck!='Unknown') %>% 
  ggplot(aes(x=Deck, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+ theme(legend.position="none")

ssex <- data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Sex, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+ theme(legend.position="none")


soddfare <- data %>% 
  filter(!is.na(Survived) & Odd_Fare!=0) %>% 
  ggplot(aes(x=Odd_Fare, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+ theme(legend.position="none")

sembark <- data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Embarked, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+ theme(legend.position="none")

stitle <- data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Title, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+ theme(legend.position="none")

sfam <- titanic %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Family_Size, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+ theme(legend.position="none")

ggarrange(sclass, ssex, sage, sfam, stitle, sembark, sdeck, soddfare, 
          ncol = 3, nrow = 3, common.legend = TRUE, legend="right") 
#_________________________________________________________________

data <- titanic
#####-----Pclass
#who composed of different classes? 
#Sex & Pclass
titanic %>% 
  ggplot(aes(x=Pclass, fill=Sex)) +
  geom_bar(position = "dodge", width = 0.4)
#Age & Pclass

# data %>% 
#   count(Pclass, Age_Group) %>% 
#   group_by(Pclass) %>% 
#   mutate(prop = n/sum(n)) %>% 
#   ggplot(aes(x = Age_Group, y = prop, fill=Age_Group)) + 
#   geom_bar(stat="identity")+
#   scale_fill_brewer(palette = 'Set2')+
#   facet_wrap(~Pclass)
  

#Age
data %>% 
  ggplot(aes(x=Age))+
  geom_boxplot()


#####-----Sex 
#men are more likely to die 


#####-----Age_Group & Sex
#who was onboard? 
data %>% ####do not like this _____________------------
  ggplot(aes(x=Sex))+
  geom_bar(position = "dodge", width = 0.4, aes(y=(..count..)/sum(..count..), )) + 
  scale_y_continuous(labels=percent)

#Survival & Age+Gender
data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Age_Group, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+
  facet_wrap(~Sex)


#women are more likely to survive - even within the same class 

#Age & Sex & Class
#first class -- older women tended to survive, but older men tended to die 
#2nd & 3rd class - those who died tended to be older than those who lived 
#interesing in male class 2: median age of survived <10 

data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Pclass, y=Age, fill=Survived))+
  geom_boxplot()+
  scale_fill_brewer(palette = 'Set1')+
  facet_wrap(~Sex)

#Survival & Age+Gender+Class ---
#overall, highlights how much of a role
#gender plays 
data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Age_Group, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4)+
  scale_fill_brewer(palette = 'Set1')+
  facet_grid(Pclass~Sex, scales = "free_y")

#same, in % terms 
# data %>% 
#   filter(!is.na(Survived) & Sex=='female') %>% 
#   ggplot(aes(x = Age_Group, fill=Survived)) +  
#   geom_bar(position = "dodge", width = 0.4, aes(y=(..count..)/sum(..count..), )) + 
#   scale_y_continuous(labels=percent)+
#   scale_fill_brewer(palette = 'Set1')+
#   facet_wrap(~Sex)
# data %>% 
#   filter(!is.na(Survived) & Sex=='male') %>% 
#   ggplot(aes(x = Age_Group, fill=Survived)) +  
#   geom_bar(position = "dodge", width = 0.4, aes(y=(..count..)/sum(..count..), )) + 
#   scale_y_continuous(labels=percent)+
#   scale_fill_brewer(palette = 'Set1')+
#   facet_wrap(~Sex)
#####-----Deck

#only 23% of the training set has Deck information - mostly for 1st class 
#occupancy & Deck 
data %>% 
  filter(!Deck=='Unknown') %>% 
  ggplot(aes(x=reorder(Deck, desc(Deck)), fill=Pclass)) +
  geom_bar(position = "dodge", width = 0.4)+
  facet_wrap(~Pclass, nrow=3) + coord_flip()+labs(x = "Deck")

data %>% 
  filter(!is.na(Survived) & !Deck=='Unknown') %>% 
  group_by(Pclass, Survived) %>% 
  count()

##cool bubbles - do not know if this is the right way to convey this 
# data %>% 
#   filter(!is.na(Survived) & !Deck=='Unknown') %>% 
#   ggplot(aes(x=Survived, y=reorder(Deck, desc(Deck)))) +
#   geom_count(aes(color=Pclass)) + 
#   facet_wrap(~Pclass) +
#   labs(y = "Deck")
  
#####-----Fare --> Fare_Per_Person
#Fare distribution 
#Fare differences were high among the different classes 
#distribution of class1 fare very high 
data %>% 
  ggplot(aes(x=Pclass, y=Fare_Per_Person, fill=Pclass))+
  geom_boxplot()+
  coord_flip()

#Survival & Fare
#if you paid more, more likely to survive - this makes sense given 
#we knew that Pclass made a difference 
data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Sex, y=Fare_Per_Person, fill=Survived))+
  geom_boxplot()+
  scale_fill_brewer(palette = 'Set1')




# data %>% 
#   filter(!is.na(Survived)) %>% 
#   ggplot(aes(x=Fare_Per_Person, fill=Survived))+
#   geom_histogram(alpha=0.8)+
#   facet_wrap(~Pclass, nrow=3, scales="free_y")


#####-----Age & Fare_Per_Person -->

data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Age_Group, y=Fare_Per_Person, fill=Survived))+
  geom_boxplot()+
  scale_fill_brewer(palette = 'Set1')

data %>% 
  filter(!is.na(Survived)) %>% 
  ggplot(aes(x=Age, y=Fare_Per_Person, color=Survived))+
  geom_jitter(size=2)+
  scale_color_brewer(palette = 'Set1')

head(data)
dim(data)
ttrain <- filter(data, !is.na(Survived))
dim(new_train)
names(ttrain)
model <- glm(Survived ~. -Age_Group-Family-N_Per_Ticket-Parch-SibSp,family=binomial(link='logit'),data=ttrain)
summary(model)
train_probs <- predict(model, data=ttrain, type="response")
table(ttrain$Survived, train_probs>0.5)
(486+267) / (486+267+63+75)
test_probs <- predict(model, newdata = ttest, type = "response")
length(test_probs)
results <- data.frame(test_probs)
dim(results)

results <- results %>% 
  mutate(Survived = case_when(test_probs>0.5 ~ 1,
                                TRUE ~ 0))

results$prediction <- NULL
final <- results[, 2:3]

171/418
predicted <- plogis(predict(logitMod, testData))  # predicted scores
vif(model)
misClassError(new_train$Survived, predicted, threshold = optCutOff)

data <- titanic[!names(titanic) %in% skip]


ttrain <- titanic[1:891,]
ttest <- titanic[892:1309,]
write.csv(ttrain,"preprocessed_train.csv")
write.csv(ttest,"preprocessed_test.csv")


dim(ttest)
sum(is.na(ttest$Survived))
dim(ttrain)
head(ttest)

final
write.csv(final,"first_submission_log.csv",  row.names = FALSE)

?write.csv
pairs(ttrain)
