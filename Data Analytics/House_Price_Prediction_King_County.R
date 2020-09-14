#Read Data from the csv file.
data_proj = read.csv('kc_house_data.csv')
head (data_proj)

#Hypothesis one tail "Average price of house in not greater than 50000" Alternate Hypothesis "Average price greater than 50000".

library(dplyr)
hs_price = data_proj$price
mean(hs_price)
describe(hs_price)

par(mfrow=c(1,1))
boxplot(hs_price, col = 'green', main = "Price of Houses")

#onetail code
onet_price = mean(hs_price)
sdev = sd(hs_price)
onet_length = length(hs_price)
onet_err = (qnorm(0.975)*sdev)/sqrt(onet_length)

onet_left = onet_price - onet_err
onet_right = onet_price + onet_err

#print values:
onet_left
onet_right

#ztest for one tail Hypothesis as sample size is large.
install.packages('BSDA')
library(BSDA)
z.test(hs_price, NULL, alternative = "greater", mu = 50000, sigma.x=sd(hs_price), conf.level = 0.95)

#Two tailed Hypothesis - Our null hypothesis is that average price of house is same for houses with 1 and 1.5 floors. However, we donot beliwve so.

hs_tt = data_proj$floors
hs_1floor = data_proj %>% filter(hs_tt==1)
hs_1.5floor = data_proj %>% filter(hs_tt==1.5)
nrow(hs_1floor)
nrow(hs_1.5floor)
price_1floor = hs_1floor$price
price_1.5floor = hs_1.5floor$price

par(mfrow=c(1,2))
boxplot(price_1floor, col="blue", main="1 Floor")
boxplot(price_1.5floor, col="green", main="1.5 Floor")

#confidence interval for 1 floor 
m_1floor = mean(price_1floor)
sd_1floor = sd(price_1floor)
len_1floor = length(price_1floor)
err2_1floor = (qnorm(0.975)*sd_1floor)/sqrt(len_1floor)
left_1floor = m_1floor - err2_1floor
right_1floor = m_1floor + err2_1floor

#confidence interval for 1.5 floor
m_1.5floor = mean(price_1.5floor)
sd_1.5floor = sd(price_1.5floor)
len_1.5floor = length(price_1.5floor)
err2_1.5floor = (qnorm(0.975)*sd_1.5floor)/sqrt(len_1.5floor)
left_1.5floor = m_1.5floor - err2_1.5floor
right_1.5floor = m_1.5floor + err2_1.5floor

#ztest for two-tail Hypothesis as sample size is large.
z.test(price_1floor, price_1.5floor,alternative = "two.sided", mu=0, sigma.x = sd(price_1floor), sigma.y = sd(price_1.5floor), conf.level = 0.95)

#Multiple-Linear Regression using 10 Fold Cross-validation:

#getting individual variables from the dataset and to plot Correlation plot we have used (80:20) due to large amount of data.

date = train.data$date
price = train.data$price
bedrooms = train.data$bedrooms
bathroom = train.data$bathrooms
sqft_living = train.data$sqft_living
sqft_lot = train.data$sqft_lot
floors = train.data$floors
waterfront = train.data$waterfront
view = train.data$view
condition = train.data$condition
grade = train.data$grade
sqft_above = train.data$sqft_above
sqft_basement = train.data$sqft_basement
yr_built = train.data$yr_built
yr_renovated = train.data$yr_renovated
zipcode = train.data$zipcode
lat = train.data$lat
long = train.data$long
sqft_living15 = train.data$sqft_living15
sqft_loft15 = train.data$sqft_lot15

data_proj= data_proj[sample(nrow(data_proj)),]
select.data = sample(1:nrow(data_proj),0.8*nrow(data_proj))
#splitting into training and testing data
train.data = data_proj[select.data,]
test.data = data_proj[select.data,]

#getting individual variables from the dataset

date = train.data$date
price = train.data$price
bedrooms = train.data$bedrooms
bathroom = train.data$bathrooms
sqft_living = train.data$sqft_living
sqft_lot = train.data$sqft_lot
floors = train.data$floors
waterfront = train.data$waterfront
view = train.data$view
condition = train.data$condition
grade = train.data$grade
sqft_above = train.data$sqft_above
sqft_basement = train.data$sqft_basement
yr_built = train.data$yr_built
yr_renovated = train.data$yr_renovated
zipcode = train.data$zipcode
lat = train.data$lat
long = train.data$long
sqft_living15 = train.data$sqft_living15
sqft_loft15 = train.data$sqft_lot15

#converting the date format and stripping out the unwanted date from the date variable.

library(lubridate)

train.data$date<-(substr(train.data$date, 1, 8))
train.data$date<- ymd(train.data$date)
train.data$date<-as.numeric(as.Date(train.data$date))

date = train.data$date

head(date)

#Correlation plot

cor(cbind(date, price, bedrooms, bathroom, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, sqft_above, sqft_basement, yr_built, yr_renovated, zipcode, lat, long, sqft_living15, sqft_loft15))

#plotting individual variables against the y variable
par(mfrow = c(4,2))
par(mar = rep(2,4))
par(mfrow=c(4,5))
plot(date, price, main = "Price vs Date", ylab = "Price", xlab = "Date", col = 'green')
plot(bedrooms, price, main = "Price vs Bedrooms", ylab = "Price", xlab = "Bedrooms", col = 'green')
plot(bathroom, price, main = "Price vs Bathroom", ylab = "Price", xlab = "Bathroom", col = 'green')
plot(sqft_living, price, main = "Price vs Sqft_Living", ylab = "Price", xlab = "Sqft_Living", col = 'green')
plot(sqft_lot, price, main = "Price vs Sqft_Loft", ylab = "Price", xlab = "Sqft_Loft", col = 'green')
plot(floors, price, main = "Price vs Floors", ylab = "Price", xlab = "Floors", col = 'green')
plot(waterfront, price, main = "Price vs Waterfront", ylab = "Price", xlab = "Waterfront", col = 'green')
plot(view, price, main = "Price vs View", ylab = "Price", xlab = "View", col = 'green')
plot(condition, price, main = "Price vs Condition", ylab = "Price", xlab = "Condition", col = 'green')
plot(grade, price, main = "Price vs Grade", ylab = "Price", xlab = "Grade", col = 'green')
plot(sqft_above, price, main = "Price vs Sqft_Above", ylab = "Price", xlab = "Sqft_Above", col = 'green')
plot(sqft_basement, price, main = "Price vs Sqft_Basement", ylab = "Price", xlab = "Sqft_Basement", col = 'green')
plot(yr_built, price, main = "Price vs Yr_Built", ylab = "Price", xlab = "Yr_Built", col = 'green')
plot(yr_renovated, price, main = "Price vs Yr_renovated", ylab = "Price", xlab = "Yr_Renovated", col = 'green')
plot(zipcode, price, main = "Price vs zipcode", ylab = "Price", xlab = "Zipcode", col = 'green')
plot(lat, price, main = "Price vs Condition", ylab = "Price", xlab = "Lat", col = 'green')
plot(long, price, main = "Price vs Long", ylab = "Price", xlab = "Long", col = 'green')
plot(sqft_living15, price, main = "Price vs Sqft_Living_15", ylab = "Price", xlab = "Sqft_Living_15", col = 'green')
plot(sqft_loft15, price, main = "Price vs Sqft_Loft_15", ylab = "Price", xlab = "Sqft_Loft_15", col = 'green')

#Correlation Matrix:

install.packages('corrplot')
library(corrplot)
library(ggplot2)
par(mfrow=c(1,1))
cplot = cor(train.data)
corrplot(cplot, type="full", method = "circle", main="Correlation")
corrplot(cplot, method = "color", outline = T, cl.pos = 'n', rect.col = "blue",  tl.col = "blue", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green","white","purple"))(100))

#For 10 Fold Cross-Validation:

#Install following Libraries:
library("car")
library(caret)
library(caTools)
library(psych)

p.data_control<-trainControl(method = "cv", number = 10)

#Building Full Model

housemodel<-train(price~bedrooms+bathrooms+sqft_living+sqft_lot+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15,
                  data = data_proj, 
                  trControl = p.data_control, 
                  method = "lm",
                  na.action = na.pass)
housemodel$finalModel
vif(housemodel$finalModel)

#After the initial Model we have removed variables sqft_living as it had vif value greater than 5.

#Rebuilding the Model again after removing sqft_living:

housemodel2<-train(price~bedrooms+bathrooms+sqft_lot+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15,
                   data = data_proj, 
                   trControl = p.data_control, 
                   method = "lm",
                   na.action = na.pass)
vif(housemodel2$finalModel)

housemodel2$resample
housemodel2$finalModel
housemodel2

#We have plotted the initial model as the Residual vs Fitted plot had no constant variance thereby applying Log Transformation to the 'y' variable price and rebuilding the model again.
plot(housemodel2$finalModel)

#Model with Log Transformation
housemodel3<-train(log(price)~bedrooms+bathrooms+sqft_lot+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15,
                   data = data_proj, 
                   trControl = p.data_control, 
                   method = "lm",
                   na.action = na.pass)
vif(housemodel3$finalModel)
housemodel3$resample
housemodel3$finalModel

housemodel3
plot(housemodel3$finalModel)

#After using Plot command we see that Residual vs Fitted plot is having constant variance and QQ Plot has most of the points on the line or near the line. 

#Random Forest Regression Technique:

library("car")
library(caret)
library(caTools)
library(psych)

install.packages("MASS")
library(MASS)

library("randomForest")

#Reading Data from CSV File. excluding date and id as they are not useful for our model building.
mydata = read.table("kc_house_data_rf.csv", header=TRUE, sep=",")

head(mydata)

#Specifying Cross-validation method for Random Forest
data_ctrl<-trainControl(method = "cv",number = 10)

s<-createDataPartition(y = mydata$price,p=0.8,list = FALSE)
training <-mydata[s,]
test <-mydata[-s,]
stopifnot(nrow(training) + nrow(test)==nrow(mydata))

x = training[,2:18]
x
y = training[,1]
y

#Use tuneRF function to calculate bestmtry value as our data is large so we have considered ntree=500.
bestmtry<- tuneRF(x,y,stepFactor=1.5,improve=1e-5,ntree=500,doBest=TRUE)

#Selecting value as '7' as best mtry value for our Random Forest Model.
pGrid<-expand.grid(mtry=c(7))

#Before Applying Transformation or Normalization Model is :

model_RF<-train(price~.,data = mydata,trControl = data_ctrl,method ="rf",tuneGrid = pGrid)

summary(model_RF)
model_RF$resample
model_RF$finalModel

model_RF

#Applying Transformation so as to increase R2 value which improves Model accuracy and reduce the error for the model.

model_RF1<-train(log(price)~.,data = mydata,trControl = data_ctrl,method ="rf",tuneGrid = pGrid)

summary(model_RF1)
model_RF1$resample
model_RF1$finalModel

model_RF1

#SVM(Support Vector Machine) Regression Technique:

library(e1071)

data_ctrl<-trainControl(method = "cv", number = 10)

svm_model <-svm(log(price)~.,data = mydata,kernel="linear")
svm_model

## Cost Factor for our Model c is 1.
psvm_grid<-expand.grid(C = c(1))


#SVM Model without Transformation

model_SVM<-train(price~.,data = mydata,trControl = data_ctrl,method ='svmLinear',tuneGrid = psvm_grid)

summary(model_SVM)
model_SVM$resample
model_SVM$finalModel

model_SVM

#Applying Transformation so as to increase R2 value which improves Model accuracy and reduce the error for the model.

model_SVM1<-train(log(price)~.,data = mydata,trControl = data_ctrl,method ='svmLinear',tuneGrid = psvm_grid)

summary(model_SVM1)
model_SVM1$resample
model_SVM1$finalModel

model_SVM1

#ANOVA Using Linear Regression:NUll hypothesis: Group mean price of all houses with different bedrooms are equal & Alternate Hypothesis:Group mean price of all houses with different bedrooms are not equal.

#factoring bedrooms data

bedroom = data_proj$bedrooms
bedroom <- as.factor(bedroom)

boxplot(hs_price~bedroom, xlab = 'Bedrooms', ylab = 'Price', main = "Price vs Bedroom", col=c("red", "green"))

#We have considered Bedroom '0' as the baseline and done comparision with other Bedroom prices.
an1 = lm(hs_price~bedroom)
summary(an1)

#From the Box plot it is evident that Bedroom 8 has more variance as compared to other bedrooms in prices so we have relevel the base to 8 from 0.

#releveling the bedroom
bedroom = relevel(bedroom, ref=8) #ref value taken as 8 as per box plot interpretation
an2 = lm(hs_price~bedroom)
summary(an2)

##Time Series Model Analysis:

#We withhold last 100 records from dataset on the basis of timestamp.
mytraindata = read.table("kc_house_traindata.csv", header=TRUE, sep=",")

head(mytraindata)
tail(mytraindata)

#Remove the unwanted data from the date part.
mytraindata$date<-(substr(mytraindata$date, 1,8))

#Apply Log Returns
rt1=log(mytraindata$price+1)
rt1

mytestdata = read.table("kc_house_testdata.csv", header=TRUE, sep=",")
head(mytestdata)

mytestdata$date<-(substr(mytestdata$date, 1,8))
mytestdata$date<- ymd(mytestdata$date)

#Creating Time-Series object with Start date as May 2014 and End date as May 2015.
hpricets1=ts(rt1,start =c(2014,05), end=c(2015,05), frequency=365)
start(hpricets1)
end(hpricets1)

hpricelogts1=log(hpricets1+1)

#Compute statistics for the same.
basicStats(rt1)
basicStats(hpricelogts1)

#Plotting Histogram to observe Normal Density Distribution for the given data
hist(hpricets1, xlab="House Price in King County", prob=TRUE)
xfit<-seq(min(hpricets1),max(hpricets1),length=40)
yfit<-dnorm(xfit,mean=mean(hpricets1),sd=sd(hpricets1))
lines(xfit, yfit, col="red", lwd=2)

#Potting QQ plot to check if the data is normally distributed or not.
qqnorm(hpricets1)
qqline(hpricets1, col = "blue")

#Plotting time-series object to check whether the plot is stationary or not.
plot(hpricets1, type='l', xlab='time', ylab='House Price in King County', main="Plot before Applying Differencing", col="blue")

plot(hpricelogts1, type='l', xlab='time', ylab='House Price Log Returns in King County', col="blue")

#Plot ACF and PACF 
acf(rt1, plot=T,lag.max=20,na.action=na.pass)
acf(rt1, plot=F,lag.max=20,na.action=na.pass)

pacf(rt1, lag = 15,na.action=na.pass)

#Using Ljung Box Test to check whether there is any White Gaussian Noise present in the data or not.
Box.test(rt1,lag=6,type = 'Ljung')
Box.test(rt1,lag=12,type = 'Ljung')
Box.test(rt1,lag=18,type = 'Ljung')

#Applying Differencing to check whether data becomes more stationary.
dhpricelogts1 = diff(hpricelogts1)
dhpricets1 = diff(hpricets1)

#plot to see whether data is stationary after applying differencing.
plot(dhpricets1, type='l', xlab='time', ylab='House Price in King County', main="Plot After Applying Differencing", col="blue")

plot(dhpricelogts1, type='l', xlab='time', ylab='House Price Log Returns in King County', col="blue")

#Compute statistics for the same.
basicStats(dhpricelogts1)

#Checking Normality Test for the House Price.
jarque.bera.test(dhpricets1)

#Plotting Histogram of differeced time-variable to observe Normal Density Distribution for the given data
hist(dhpricets1, xlab="House Price in King County", prob=TRUE)
xfit<-seq(min(dhpricets1),max(dhpricets1),length=40)
yfit<-dnorm(xfit,mean=mean(dhpricets1),sd=sd(dhpricets1))
lines(xfit, yfit, col="red", lwd=2)

#Potting QQ plot to check if the data is normally distributed or not.
qqnorm(dhpricets1)
qqline(coredata(dhpricets1), col = "blue")

#Plot ACF(q value for MA) and PACF(p value for AR)
acf(coredata(dhpricets1), plot=T,lag.max=20,na.action=na.pass)  
acf(coredata(dhpricets1), plot=F,lag.max=20,na.action=na.pass)

pacf(coredata(dhpricets1),lag=20)

#Using Ljung Box Test to check whether there is any White Gaussian Noise present in the data or not.
Box.test(coredata(dhpricets1),lag=6,type = 'Ljung')
Box.test(coredata(dhpricets1),lag=12,type = 'Ljung')
Box.test(coredata(dhpricets1),lag=18,type = 'Ljung')


#Building AR Model:
m2_AR=arima(dhpricets1, c(1,0,0))
m2_AR

rate2 = mytestdata$price
rate2 = log(rate2)
rate2

###Predicitng Model Accuracy wrt RMSE and MAE Values:
accuracy(forecast(m2_AR),rate2)

##Forecasting for Future House Prices:
predict(m2_AR,n.ahead=10,se.fit=T)

#Building MA Model:
m2_MA=arima(dhpricets1,order = c(0,0,1))
m2_MA

#Build Model for ARMA using EACF Matrix
source("EACF.R")

EACF(dhpricets1)
#p=1 and q=2

m2_ARMA=arima(dhpricets1, order=c(1,0,2),method='ML',include.mean=T)
m2_ARMA

#Building ARIMA Model using auto-arima function

library(forecast)

m2_ARIMA=auto.arima(dhpricets1,max.P=12,max.Q=12,ic="aic")
m2_ARIMA

#Compute MAE, RMSE and Predict the future prices.
accuracy(forecast(m2_MA),rate2)
predict(m2_MA,n.ahead=10,se.fit=T)

accuracy(forecast(m2_ARMA),rate2)
predict(m2_ARMA,n.ahead=10,se.fit=T)

accuracy(forecast(m2_ARIMA),rate2)
predict(m2_ARIMA,n.ahead=10,se.fit=T)

#Residual Analysis for AR Model:
Box.test(m2_AR$residuals,lag=6,type='Ljung')
Box.test(m2_AR$residuals,lag=12,type='Ljung')
Box.test(m2_AR$residuals,lag=18,type='Ljung')

#Create Residual Plot for AR Model:
tsdiag(m2_AR)
plot(m2_AR$residuals,type='l')
qqnorm(m2_AR$residuals)
qqline(m2_AR$residuals,col=2)

#Create Residual Plot for ARMA Model:
tsdiag(m2_ARMA)
plot(m2_ARMA$residuals,type='l')
qqnorm(m2_ARMA$residuals)
qqline(m2_ARMA$residuals,col=2)

#To check whether Residue is present for ARMA Model or not using ACF/PACF Plot.
acf(coredata(m2_ARMA$residual), plot=T,lag.max=20)
pacf(coredata(m2_ARMA$residual), plot=T,lag.max=20)

#Residual Analysis for ARMA Model:
Box.test(m2_ARMA$residuals,lag=6,type='Ljung')
Box.test(m2_ARMA$residuals,lag=12,type='Ljung')
Box.test(m2_ARMA$residuals,lag=18,type='Ljung')

#Create Residual Plot for MA Model:
tsdiag(m2_MA)
plot(m2_MA$residuals,type='l')
qqnorm(m2_MA$residuals)
qqline(m2_MA$residuals,col=2)

#To do check whether Residual Analysis is true for the Model selected for house price prediction.
Box.test(m2_MA$residuals,lag=6,type='Ljung')
Box.test(m2_MA$residuals,lag=12,type='Ljung')
Box.test(m2_MA$residuals,lag=18,type='Ljung')