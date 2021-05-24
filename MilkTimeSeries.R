library(MASS)
library(qpcR)
library(sarima)
library(astsa)
source("spec.arma.R")
source("plot.roots.R")
milk <- read.csv("milk.csv")
colnames(milk)[2] <- "production"
prod<- milk$production[-169]
ts.plot(prod)
#Data is not stationary(clear upward trend) and seasonality component
#Variance looks to be constant 
#Apply box cox even though variance looks to be constant

#Box Cox
require(MASS)
bctrans <- boxcox(prod~as.numeric(1:length(prod)),
                  plotit=TRUE,
                  lambda=seq(-4,2,0.1))
lambda <- bctrans$x[which(bctrans$y==max(bctrans$y))]
lambda  
#lamda = .42 which is close to .5 so should try sqrt transformation
#zero is also in the interval so lets try log transformation as well
prod.sqrt <- sqrt(prod)
prod.log <- log(prod)

# Plot original data vs Box-Cox transformed data
par(mfrow=c(1,2))
ts.plot(prod,main = "Original data",ylab = expression(X[t]))
ts.plot(prod.sqrt,main = "Sqrt tranformed data", ylab = expression(Y[t]))
#Does not seem to be much of a difference between sqrt transformed and orginal
#Try plotting orginal vs log transformed
ts.plot(prod,main = "Original data",ylab = expression(X[t]))
ts.plot(prod.log,main = "Log tranformed data", ylab = expression(Y[t]))
#Not much of a difference in this case either
#Decide to not pursue transformation further
#Just work with original non transformed data

par(mfrow = c(1,1))

#Lets plot acf and pacf of milk production data
acf(prod, lag.max=60, main="ACF for production data")
#From acf can tell that data is defintely non stationary
pacf(prod ,lag.max=60, main="PACF for production data")
#pacf also suggests data is non stationary

#Differencing once at lag 1 to remove trend
milk.diff1 <- diff(prod, lag = 1)
var(prod)
var(milk.diff1) #Variance goes down which is good
ts.plot(milk.diff1) #No trend is evident now, seasonality is still apparent though
acf(milk.diff1 ,lag.max=60, main="ACF for production data")
#Seasonal component is apparent with ACF, notice pattern of spikes at 12, 24,...
pacf(milk.diff1 ,lag.max=60, main="PACF for production data")
#Lots of spikes in PACF before 12

#Difference again at lag 12 to remove seasonality
milk.diff12 <- diff(milk.diff1, lag=12)
var(milk.diff1)
var(milk.diff12) #Variance goes down which means differencing again was a valid option
par(mfrow=c(1,1))
plot.ts(milk.diff1, main = "Plot for data difference once")
plot.ts(milk.diff12, main = "Plot for data differnced twice") 
#Patttern of spikes is gone which suggests seasonality is removed
#Also notice that variance is significantly lower
acf(milk.diff12 ,lag.max=60, main="ACF: Seasonality and Trend Removed")
#Acf suggests that data is stationary
#Possible cut off at 12, so Q= 1, cut off at 1 so q = 1
pacf(milk.diff12 ,lag.max=60, main="PACF: Seasonality and Trend Removed")
#Pacf suggests that data is stationary
#Cut off at 12, ,24 ,36  so P = 1, 2 or 3
#Cut off at 1 so p = 1
#d = 1, D = 1, P = (1,2,3), Q=1, p=1, q  = 1

#Sarima (1,1,1) x (1,1,1) 12
#Sarima (1,1,1) x (2,1,1) 12
#Sarima (1,1,1) x (3,1,1) 12

fit1 <- arima(x=prod, order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12))
fit1 #AIC = 1069.78
fit2 <- arima(x=prod, order=c(1,1,1),seasonal=list(order=c(2,1,1),period=12))
fit2  #AIC  = 1071.5
fit3 <- arima(x=prod, order=c(1,1,1),seasonal=list(order=c(3,1,1),period=12))
fit3 #AIC = 1068.68
# fit3 has lowest AIC value of 1068.68

#Before continuing make sure to check causaulity and invertiblity of the models
arma.spec(ar=c(-0.1394,-.0645), ma=c(-.0942,-.5840))

plot(polyroot(c(-.1394,-0.0645)),main="roots of AR part - fit1")
plot(polyroot(c(-.0942,-0.5840)),main="roots of MA part - fit1")
plot(polyroot(c(-.1328,-0.1211,-.0630)),main="roots of AR part - fit2")
plot(polyroot(c(-.0959,-0.5287)),main="roots of MA part - fit2")
plot(polyroot(c(-.1559,-0.5998,-.3691,-.2967)),main="roots of AR part - fit3")
plot(polyroot(c(-.0454,-0.0595)),main="roots of MA part - fit3")




