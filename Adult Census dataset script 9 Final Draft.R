shell("cls")

# 1. INTRODUCTION

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(htmlTable)) install.packages("htmlTable", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(readxl)
library(knitr)
library(htmlTable)
library(rpart)
library(rpart.plot)
library(randomForest)


options(pillar.sigfig = 5)

# Adult Census Income:
# https://archive.ics.uci.edu/ml/machine-learning-databases/adult/
# https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
# https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test

# 2. ANALYSIS

# 2.1 THE CENSUS ADULT INCOME DATA SET

# US cencus data
census_uci <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                     sep = ',', fill = F, strip.white = T)
colnames(census_uci) <- c('age', 'workclass', 'fnlwgt', 'education', 
                      'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                      'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')

census <- census_uci

# kabble(census)
glimpse(census)
# view(census)

# 2.1.1 Summary tables and plots Adult Census Income data set
 census %>% ggplot(aes(x=age,color=income,fill =income))+geom_histogram(color="black",binwidth = 5)+ggtitle("Age")
 census %>% group_by(age) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=workclass,color=income,fill =income))+geom_bar(color="black")+ggtitle("Work Class") + theme(axis.text.x = element_text(angle = 90))
 census %>% group_by(workclass) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=fnlwgt,color=income,fill =income))+geom_histogram(color="black")+ggtitle("Fnlwgt")+scale_x_continuous(trans = 'log10')
 census %>% group_by(fnlwgt) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=education,color=income,fill =income))+geom_bar(color="black")+ggtitle("Education Level") + theme(axis.text.x = element_text(angle = 90))
 census %>% group_by(education) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=education_num,color=income,fill =income))+geom_histogram(color="black",binwidth = 1)+ggtitle("Years of Education")
 census %>% group_by(education_num) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=marital_status,color=income,fill =income))+geom_bar(color="black")+ggtitle("Marital Status") + theme(axis.text.x = element_text(angle = 90))
 census %>% group_by(marital_status) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=occupation,color=income,fill =income))+geom_bar(color="black")+ggtitle("Occupation") + theme(axis.text.x = element_text(angle = 90))
 census %>% group_by(occupation) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=relationship,color=income,fill =income))+geom_bar(color="black")+ggtitle("Relationship") + theme(axis.text.x = element_text(angle = 90))
 census %>% group_by(relationship) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=race,color=income,fill =income))+geom_bar(color="black")+ggtitle("Race") + theme(axis.text.x = element_text(angle = 90))
 census %>% group_by(race) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=sex,color=income,fill =income))+geom_bar(color="black")+ggtitle("Sex")
 census %>% group_by(sex) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=capital_gain,color=income,fill =income))+geom_histogram(color="black")+ggtitle("Capital Gains")
 census %>% group_by(capital_gain) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=capital_loss,color=income,fill =income))+geom_histogram(color="black")+ggtitle("Capital Losses")
 census %>% group_by(capital_loss) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=hours_per_week,color=income,fill =income))+geom_histogram(color="black",binwidth = 5)+ggtitle("Hours Per Week")
 census %>% group_by(hours_per_week) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=native_country,color=income,fill =income))+geom_bar(color="black")+ggtitle("Native Country") + theme(axis.text.x = element_text(angle = 90))
 census %>% group_by(native_country) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))
 
 census %>% ggplot(aes(x=income,color=income,fill =income))+geom_bar(color="black")+ggtitle("Income Bracket")
 census %>% group_by(income) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(Pct=n/sum(n))

#Inspecting different variables for redundancy
#education vs Education_num. Remove Education field due to 1:1 relationship with education_num
census %>% group_by(education_num,education) %>% summarise(n=n()) %>% arrange(education_num)

#Relationshio vs sex vs marital_status
census %>% group_by(marital_status,relationship) %>% summarise(n=n()) %>% arrange(marital_status) 


#Redefine data set removing insignificant columns
# Remove EDUCATION field: redundant
# Remove RELATIONSHIP field: relevant info provided by SEX and MARITAL_STATUS fields
# Remove FNLWGT field: not relevant to final results
# RACE field grouped into WHITE= TRUE
# AMERICAN: United-States = TRUE, others = FALSE
# CAPITAL_GAIN / LOSS: >0 = TRUE
# Income_Class: >50k = 1, <=50k =0
# Sex: F=0, M=1
# OCCUPATION field grouped into PROF_CLERICAL = TRUE
# MARITAL_STATUS field grouped into MARRIED = TRUE
# Remove WORKCLASS: unresolvable error

census <- census_uci %>% 
 mutate(
    # marital_status= ifelse(str_detect(marital_status,"Married"),"Married",str_c(marital_status))
     married= str_detect(marital_status,"Married")
    ,prof_clerical = (occupation=="Prof-specialty" | 
                        occupation=="Exec-managerial"|
                        occupation=="Adm-clerical"|
                        occupation=="Sales"|
                        occupation=="Tech-support"|
                        occupation=="Protective-serv")
     ,sex = ifelse(sex=="Male",1,0)
     ,American = str_detect(native_country,"United-States")
     ,capital_gain=(capital_gain>0)
     ,capital_loss=(capital_loss>0)
     # ,white = race == "white"     
     ,income_class = (income==">50K")*1
    ) %>% 
  # select(-education,-fnlwgt,-relationship,-capital_gain,-capital_loss,-native_country)
  select(-education,-fnlwgt,-native_country,-relationship,-workclass,-marital_status,-occupation, ) 

str(census)


# Create Data partition
 set.seed(1)

test_index <- createDataPartition(y = census$income, times = 1, p = 0.2, list = FALSE)
train_set <- census[-test_index,]
test_set <- census[test_index,]

table(train_set$income_class)

#--------------
# ANALYSIS

#--------------
# 2.2 GLM LINEAR REGRESSION MODEL
train_glm <- train_set %>% 
  select(-income) %>% 
  glm(income_class ~ .,data = .,family=binomial('logit')) 

#GLM Summary
train_glm%>% 
  summary()

#GLM coefficients
train_glm

# Remove original INCOME field 
test_set2 <- test_set %>% 
  select(-income)

train_set2 <- train_set %>% 
  select(-income)

# predict(train_glm,test_set2,type='response')

#Fit to Test set
# fit <- round(predict(train_glm,test_set2,type='response'),0)
fit <- ifelse(predict(train_glm,test_set2,type='response')>=0.5,1,0)

fit_table <- data.frame(test_set,fit)

str(fit_table)


# Accuracy of the the linear regression fit
table(fit_table$income_class,fit_table$fit)
nrow(fit_table )
table(fit_table$income_class,fit)/nrow(fit_table )

GLM_acc<-fit_table %>% 
  summarise(mean(income_class==fit))

# GLM_acc
tibble(Method = "Generalised Linear Model", Accuracy = as.numeric(GLM_acc))

#--------------
# 2.3 K NEAREST NEIGHBOURS

#Remove MARITAL_SATAUS due to unresolvable error
train_set5 <- train_set %>% 
  # select(-income,-marital_status,-occupation)
  select(-income_class)

test_set5 <- test_set %>% 
  # select(-income,-marital_status,-occupation)
  select(-income_class)

train_knn <- train(income~.,method="knn",data = train_set5)

#KNN model parameters
train_knn$bestTune
train_knn$finalModel

knn_pred<-predict(train_knn,test_set5,type="raw")

knn_table <- data.frame(test_set5,knn_pred)

# Accuracy of the the KNN fit
table(knn_pred,test_set5$income)
length(knn_pred)
table(knn_pred,test_set5$income)/length(test_set5$income)

KNN_acc<-knn_table %>% 
	summarise(mean(income==knn_pred))

# KNN_acc
tibble(Method = "K Nearest Neighbours", Accuracy=as.numeric(KNN_acc))

#--------------
# 2.4 REGRESSION TREES

#Remove categorical fields
train_set3 <- train_set %>% 
  # select(-income,-marital_status,-occupation)
  select(-income)

test_set3 <- test_set %>% 
  # select(-income,-marital_status,-occupation)
  select(-income)

#Probability Plot of earning GT 50K

fit_rt <- rpart(income_class~.,data = train_set3)
# fit_rt

rpart.plot(fit_rt,main="Probability of GT $50K",yesno=2)
rpart.rules(fit_rt,cover=TRUE)


#Classiification Plot of earning GT 50K
fit_rtc <- rpart(income_class~.,data = train_set3,method = 'class')

rpart.plot(fit_rtc, main="Classification of GT $50K",yesno=2)
rpart.rules(fit_rtc,cover=TRUE)

#Type = class
 rt_predc <- predict(fit_rtc,newdata=test_set3,type='class')
# rt_predc <- predict(fit_rtc,newdata=test_set3)

rt_table <- table(rt_predc,test_set3$income_class)

rt_table
nrow(test_set3)
rt_table/nrow(test_set3)
rt_dftable <- data.frame(test_set3,rt_predc)

rt_acc <- rt_dftable %>% summarise(mean(income_class==rt_predc))
# rt_acc
tibble(Method = "Regression Trees", Accuracy = as.numeric(rt_acc))

#---------------------
# 2.5 RANDOM FORESTS

#Remove MARITAL_SATAUS due to unresolvable error
train_set4 <- train_set %>% 
  # select(-income,-marital_status,-occupation)
   # select(-income,-marital_status)
   select(-income)
  
test_set4 <- test_set %>% 
  # select(-income,-marital_status,-occupation)
  # select(-income,-marital_status)
   select(-income)


#train_rf <- randomForest(income_class~.,data=train_set4,trees= 150)
 train_rf <- randomForest(income_class~.,data=train_set4)
train_rf


#Accuracy improves with the number of trees
plot(train_rf)

rf_pred <- ifelse(predict(train_rf,test_set4)>=0.5,1,0)

rf_table <- table(rf_pred,test_set4$income_class)

rf_table
nrow(test_set4)
rf_table/nrow(test_set)
rf_dftable <- data.frame(test_set4,rf_pred)

rf_acc <- rf_dftable %>% summarise(mean(income_class==rf_pred))
# rf_acc
tibble(Method = "Random Forests", Accuracy = as.numeric(rf_acc))

#--------
# accuracy summary

# 3. RESULTS
# Training set of 20% of data set

results1 <- tibble(Method = "Generalised Linear Model", Accuracy = as.numeric(GLM_acc))
results2 <- tibble(Method = "K Nearest Neighbours", Accuracy=as.numeric(KNN_acc))
results3 <- tibble(Method = "Regression Trees", Accuracy = as.numeric(rt_acc))
results4 <- tibble(Method = "Random Forests", Accuracy = as.numeric(rf_acc))

rbind(results1,results2,results3,results4)

