
# install packages

install.packages('forecast')
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('ggfortify')

# call packages

library('forecast')
library('ggplot2')
library('ggthemes')
library('ggfortify')

# Read Data

bicup.df <- read.csv("C:/Users/Sandhya Ravindran/Desktop/r_wd/bicup2006.csv")

# Lets start by creating a new column for the day of week and factorize it.

day.of.week <- rep(rep(c(1:7),each = 63),4)
bicup.df$DAY.OF.WEEK <- days[1:dim(bicup.df)[1]]
bicup.df$DAY.OF.WEEK <- as.factor(bicup.df$DAY.OF.WEEK)
levels(bicup.df$DAY.OF.WEEK)

# Lets create our time series object and plot

bicup.ts <- ts(bicup.df$DEMAND, start =c(1,1), end = c(21,63), freq = 63)
#autoplot(bicup.ts,ts.colour = '#40e0d0', ts.linetype = 'dashed')
autoplot(bicup.ts,ts.geom = 'ribbon', fill= '#c6e2ff')

# Data Partition 

train.ts <- window(bicup.ts, start = c(2,1), end = c(18,63))
valid.ts <- window(bicup.ts, start = c(19,1), end = c(21,63))
train.df <- bicup.df[1:18*63,]
next3days.df <- bicup.df[(21*63+1):(24*63),]
head(train.df)
head(next3days.df)

# Fitting model
train.df$DATE
train.df$DAY.OF.WEEK

bicup.lm <- tslm(train.ts~trend , data = train.df)
bicup.lm.pred <- forecast(bicup.lm, h= 63*3, level=0,newdata = train.df$DAY.OF.WEEK)
accuracy(valid.ts,bicup.lm.pred$mean)

autoplot(bicup.lm.pred, ts.colour = '#868f98', predict.colour = '#d57946',
         predict.linetype = 'dashed', conf.int = FALSE)

bicup.lm.fulldata <- tslm(bicup.ts~trend, data = bicup.df[1:(21*63),])
bicup.lm.next3days <- forecast(bicup.lm.fulldata, h=63*3,level=0,newdata=next3days.df)
autoplot(bicup.ts, ts.colour = '#868f98', xlab="time",ylab = "Demand",xlim = c(0,25)) + autolayer(bicup.lm$fitted) +autolayer(bicup.lm.next3days$mean)+autolayer(bicup.lm.pred$mean)
