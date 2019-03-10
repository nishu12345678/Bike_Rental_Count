rm(list=ls())
getwd()

#Loading Required Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"usdm","scales","psych","gplots")

install.packages(x)
lapply(x, require, character.only = TRUE)

#Loading CSV file
bike_rental = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))

#Converting the required numerical variables to factor variables:
catnames=c("season","yr","mnth","holiday","weekday","workingday","weathersit")
for(i in catnames){
  print(i)
  bike_rental[,i]=as.factor(bike_rental[,i])
}
str(bike_rental)
bike_rental$dteday=NULL
bike_rental$instant=NULL
#Missing Value Analysis
sum(is.na(bike_rental))
##No missing values are present in the given data set.

#Outlier Analysis
class(bike_rental)
num_index=sapply(bike_rental, is.numeric)
numeric_data=bike_rental[,num_index]
num_cnames=colnames(numeric_data)
hist(bike_rental$temp,main = "Temperature",col = (c("lightblue","darkgreen")))
hist(bike_rental$atemp,main = "ATemperature",col = (c("lightblue","darkgreen")))
hist(bike_rental$hum,main = "Humidity",col = (c("lightblue","darkgreen")))
hist(bike_rental$windspeed,main = "Windspeed",col = (c("lightblue","darkgreen")))
hist(bike_rental$casual,main = "Casual",col = (c("lightblue","darkgreen")))
hist(bike_rental$registered,main = "Registered",col = (c("lightblue","darkgreen")))
hist(bike_rental$cnt,main = "Count",col = (c("lightblue","darkgreen")))
num_cnames=num_cnames[num_cnames!="cnt"]
num_cnames
for (i in 1:length(num_cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (num_cnames[i]), x = "cnt"), data = subset(bike_rental))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=num_cnames[i],x="cnt")+
           ggtitle(paste("Box plot of count for",num_cnames[i])))
}

# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)

gridExtra::grid.arrange(gn4,gn5,ncol=2)
gridExtra::grid.arrange(gn6,gn3,ncol=2)
for(i in num_cnames){
  print(i)
  val = bike_rental[,i][bike_rental[,i] %in% boxplot.stats(bike_rental[,i])$out]
  print(length(val))
  
}
#Replace all outliers with NA and impute

for(i in num_cnames){
  val = bike_rental[,i][bike_rental[,i] %in% boxplot.stats(bike_rental[,i])$out]
  print(length(val))
  bike_rental[,i][bike_rental[,i] %in% val] = NA
}
sum(is.na(bike_rental))
bike_rental = knnImputation(bike_rental, k = 7)
sum(is.na(bike_rental))

#Plotting Bar Plot together
install.packages("scales")
library("ggplot2")
library(scales)
library("psych")
library("gplots")

#Plot of month vs count
ggplot(bike_rental, aes_string(x = bike_rental$mnth,y=bike_rental$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Month") + ylab('Count') +
  ggtitle("Distribution of count by months") +  theme(text=element_text(size=12))

#PLot of seasons vs count
ggplot(bike_rental, aes_string(x = bike_rental$season,y=bike_rental$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Seasons") + ylab('Count') +
  ggtitle("Distribution of count by seasons") +  theme(text=element_text(size=12))


#Plot of weekday vs count
ggplot(bike_rental, aes_string(x = bike_rental$weekday,y=bike_rental$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Weekday") + ylab('Count') +
  ggtitle("Distribution of count by weekday") +  theme(text=element_text(size=12))



#Plot of weathersit vs count
ggplot(bike_rental, aes_string(x = bike_rental$weathersit,y=bike_rental$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Weathersit") + ylab('Count') +
  ggtitle("Distribution of count by weathersit") +  theme(text=element_text(size=12))


#Plot of yr vs count
ggplot(bike_rental, aes_string(x = bike_rental$yr,y=bike_rental$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Year") + ylab('Count') +
  ggtitle("Distribution of count by Year") +  theme(text=element_text(size=12))




#Feature Selection
#Correlation Plot
corrgram(bike_rental,upper.panel = panel.pie,text.panel = panel.txt,main="correlation_plot")
#As temp and atemp are highly correlated so one variable need to be remove, so atemp was removed
#ANOVA test
for (i in catnames) {
  print(i)
  print(summary(aov(bike_rental$cnt~bike_rental[,i],bike_rental)))
  
}
bike_rental=subset(bike_rental,select=-c(weekday,atemp))
#Checking from variance inflation factor
numeric_data=subset(numeric_data,select=-c(atemp,cnt))

library(usdm)
vif(numeric_data)
vifcor(numeric_data,th=0.9)

#From the Variance inflation factor we can clearly see that no variable is having multicollinearity issues


#Feature Scaling
fea_names=c("casual","registered")
for (i in fea_names) {
  bike_rental[,i]=(bike_rental[,i]-min(bike_rental[,i]))/(max(bike_rental[,i])-min(bike_rental[,i]))
  
}
#Plotting Histogram after preprocessing of Data
hist(bike_rental$temp,main = "Temperature",col = (c("lightblue","darkgreen")))
hist(bike_rental$hum,main = "Humidity",col = (c("lightblue","darkgreen")))
hist(bike_rental$windspeed,main = "Windspeed",col = (c("lightblue","darkgreen")))
hist(bike_rental$casual,main = "Casual",col = (c("lightblue","darkgreen")))
hist(bike_rental$registered,main = "Registered",col = (c("lightblue","darkgreen")))
hist(bike_rental$cnt,main = "Count",col = (c("lightblue","darkgreen")))


#Model Development
set.seed(123)
X_index=sample(1:nrow(bike_rental),0.8*nrow(bike_rental))
X_train=bike_rental[X_index,-12]
X_test=bike_rental[-X_index,-12]
Y_train=bike_rental[X_index,12]
Y_test=bike_rental[-X_index,12]

train=bike_rental[X_index,]
test=bike_rental[-X_index,]

#Calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}
###########################Multiple Linear Regression###################
lm_model = lm(cnt ~., data = train)
summary(lm_model)
#Predict for new test cases
cat_index=sapply(bike_rental, is.factor)
cat_data=bike_rental[,cat_index]
cat_cnames=colnames(cat_data)
cat_cnames
for (i in cat_cnames) {
  lm_model$xlevels[[i]]=union(lm_model$xlevels[[i]],levels(X_test[[i]]))
  
}
lm_predict=predict(lm_model,newdata = X_test)
lm_predict
MAPE(lm_predict,Y_test)
######################Decesion Tree Regression#####################
DT_model=rpart(cnt ~.,data=train,method = "anova")
#Predict for new test cases
DT_predict=predict(DT_model,X_test)
summary(DT_model)
MAPE(DT_predict,Y_test)


#####################Random Forest###############
RF_model=randomForest(x=X_train,y=Y_train,ntree = 100)
#Predict for new test cases
RF_predict=predict(RF_model,X_test)
getTree(RF_model,1,labelVar = TRUE)
summary(RF_model)

MAPE(RF_predict,Y_test)
