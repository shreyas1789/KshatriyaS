library(dplyr) # library for manipulation with data 
library(DMwR) # for data mining
library(rpart) # for decision tree(regression)
library(randomForest) # for random forest 
library(usdm) # for uncertainty analysis for species distribution model
library(inTrees) # for extracting trees 


setwd("D:/Shreyas") # for setting the weorking directory
getwd()             # for get to know the working directory 
df_train<-read.csv("train_cab.csv") #saving the train_cab file in df_train 
df_test<- read.csv("test.csv")     #saving the test file in df_test 
str(df_test)                       #to know the strings of test & train columns
str(df_train)
df_train$fare_amount<- as.numeric(df_train$fare_amount) #to convert from factor to numeric string 
df_train$fare_amount<- abs(df_train$fare_amount) #as in fare_amount columns, there are negative numbers, so absolute them. 
#outliers analysis
boxplot(df_train)
df_train<- df_train[,-2] # pickup_date is a factor, and nothing use of that so ignored that column.
cnames=colnames(df_train) # saving the column names in cnames.
#outliers analysis
boxplot(df_train) # there are outliers in pickup_ latitude , pickup_longitude, dropoff_latitude, dropoff_longitude & passenger_count
for(i in cnames){ print(i)  #for loop for the outliers in the data, to convert them into NA's   
val=df_train[,i][df_train[,i]%in%boxplot.stats(df_train[,i])$out]
df_train[,i][df_train[,i] %in% val]=NA
print(length(val))}

#[1] "fare_amount"
#[1] 0
#[1] "pickup_longitude"
#[1] 1114
#[1] "pickup_latitude"
#[1] 791
#[1] "dropoff_longitude"
#[1] 1174
#[1] "dropoff_latitude"
#[1] 1007
#[1] "passenger_count"
#[1] 1696


sum(is.na(df_train))
df_train=knnImputation(df_train,k=3) # here the error occured that insufficient.
#median method to convert NA's to median value by for loop.
for(i in cnames) 
{
df_train[,i][is.na(df_train[,i])]=median(df_train[,i],na.rm=T)
}
View(df_train)
sum(is.na(df_train))
# sampling method for splitting the data.
train_index<- sample(nrow(df_train),0.38*nrow(df_train) )
train= df_train[train_index,]
test= df_train[-train_index,]
# decision tree by anova method
fit_dt<- rpart(fare_amount~., train, method="anova")
pred_dt<- predict(fit_dt, test[,-1])

MAPE= function(p,a) # mape is the measures of prediction accuracy.
 mean(abs((a-p)/a))*100

RMSE= function(p,a)
 sqrt(mean((p-a)**2))

MAPE(pred_dt,test[,1])   #316.94
RMSE(pred_dt, test[,1])  #156.34
#linear Regression

lm_model= lm(fare_amount~., data=train)
summary(lm_model)
pred_lm= predict(lm_model,test[,-1])

MAPE(pred_lm,test[,1])  # 353.52
RMSE(pred_lm,test[,1])  # 170.67
rf1_md=randomForest(fare_amount~., train, importance=TRUE, ntree=100)
tr1=RF2List(rf1_md)
ex1= extractRules(tr1,train[,-1])
ex1[1:2,]
#[1] "X[,2]<=40.714136 & X[,4]<=40.757754 & X[,5]<=0.56 & X[,5]<=0.06"                  
#[2] "X[,2]>40.714136 & X[,2]<=40.720475 & X[,4]<=40.757754 & X[,5]<=0.56 & X[,5]<=0.06"

rdrule= presentRules(ex1, colnames(df_train))
rdrule[1:2,]
#[1] "pickup_longitude<=40.714136 & dropoff_longitude<=40.757754 & dropoff_latitude<=0.56 & dropoff_latitude<=0.06"                             
#[2] "pickup_longitude>40.714136 & pickup_longitude<=40.720475 & dropoff_longitude<=40.757754 & dropoff_latitude<=0.56 #& dropoff_latitude<=0.06"

rlmetr= getRuleMetric(ex1,train[,-1],train$fare_amount)
rlmetr[1:2,]
#  len freq err
#[1,] "4" "0"  "0"
#[2,] "5" "0"  "0"
#     condition                                                                          
#[1,] "X[,2]<=40.714136 & X[,4]<=40.757754 & X[,5]<=0.56 & X[,5]<=0.06"                  
#[2,] "X[,2]>40.714136 & X[,2]<=40.720475 & X[,4]<=40.757754 & X[,5]<=0.56 & X[,5]<=0.06"
#     pred 
#[1,] "34" 
#[2,] "418"

pred_rf= predict(rf1_md,test[,-1])

MAPE(pred_rf,test[,1])  # 303.10
RMSE(pred_rf,test[,1])  # 145.82
# RMSE is bigger so, I want to use here standardisation,
#standardisation
for (i in cnames)
{   df_train[,i]= (df_train[,i]-mean(df_train[,i]))/sd(df_train[,i])
}
train_index<- sample(nrow(df_train),0.38*nrow(df_train) )
train= df_train[train_index,]
test= df_train[-train_index,]
rf1_md=randomForest(fare_amount~., train, importance=TRUE, ntree=15)
pred_rf= predict(rf1_md,test[,-1])
MAPE(pred_rf,test[,1])  # 104.53
RMSE(pred_rf,test[,1])  #0.86
# Here by standardisation , got rmse 0.86.

write.xlsx(df_test1, "E:/df_test1R.xlsx")
