# load packages
if (!require(pacman)) install.packages('pacman')
p_load(tidyverse)

# read in data
setwd("/Users/abdullah/Desktop")
dat <- read.csv('data.csv', header=TRUE)

# fix column names
names(dat) <- c('Screen Brightness', 'Source of Internet', '# Background Apps',
                'Night Shift', 'Power Saving Mode', 'Foreground Application',
                'Bluetooth', 'Phone', 'Time')

# # set column levels
# dat <-
#   mutate(dat, 
#          `Screen Brightness`=factor(`Screen Brightness`, levels=c('Low', 'High')),
#          `Source of Internet`=factor(`Source of Internet`, levels=c('Wi-Fi', 'Cellular Data')),
#          `# Background Apps`=factor(`# Background Apps`, levels=c('0', '5')),
#          `Night Shift`=factor(`Night Shift`, levels=c('Off', 'On')),
#          `Power Saving Mode`=factor(`Power Saving Mode`, levels=c('Off', 'On')),
#          `Foreground Application`=factor(`Foreground Application`, levels=c('Tinder', 'Pokemon GO')),
#          `Bluetooth`=factor(`Bluetooth`, levels=c('Off', 'On')),
#          `Phone`=factor(`Phone`, levels=c('iPhone SE', 'iPhone 6+')))
# 
# # convert data to long format for plotting
# plot.labels <-
#   mutate_each(dat, funs(as.character), -Time) %>%
#   gather(key='Treatment Factor', value='Factor Levels', -Time) %>%
#   group_by(`Treatment Factor`, `Factor Levels`) %>%
#   summarise(`Level Means`=mean(Time)) %>%
#   ungroup()
# 
# plot.dat <- 
#   group_by(plot.labels, `Treatment Factor`) %>%
#   summarise(ymin=min(`Level Means`),
#             ymax=max(`Level Means`)) %>% 
#   arrange(ymax) %>%
#   mutate(`Treatment Factor`=factor(`Treatment Factor`, levels=`Treatment Factor`))
# 
# plot.labels <-
#   mutate(plot.labels,
#          `Treatment Factor`=factor(`Treatment Factor`, levels=plot.dat$`Treatment Factor`)) %>%
#   arrange(`Level Means`) %>%
#   arrange(`Treatment Factor`)
# 
# plot.labels.low <- 
#   plot.labels[seq(1, 16, by=2), ]
# 
# plot.labels.high <- 
# 
# # plot level means
# ggplot(plot.labels, mapping=aes(x=`Treatment Factor`, y=`Level Means`)) +
#   # add level means and their labels
#   geom_point(size=0.5, alpha=0.5) +
#   geom_linerange(data=plot.dat, mapping=aes(x=`Treatment Factor`, ymin=ymin, ymax=ymax), inherit.aes=FALSE) +
#   geom_text(aes(label=`Factor Levels`), position=position_dodge(width=10)) +
#   # add overall mean
#   geom_point(aes(y=mean(`Level Means`)), shape=4) +
#   geom_line(aes(y=mean(`Level Means`), group=1)) +
#   # adjust x axis label angle
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
# 
# # plot interaction effect between screen brightness and foreground app
# interaction.plot(dat$`Screen Brightness`, dat$`Foreground Application`, dat$Time)

# fit anova model
aov.out <- aov(Time ~ .*., data=dat)
summary(aov.out)

lm(Time ~ .*., data=dat) %>% summary()
confint(lm(Time ~ .*., data=dat))

#plot 
interaction.plot(dat$`Power Saving Mode`, trace.factor = dat$`Foreground Application` ,response = dat$Time)
plot(aov.out)

#Model Checking 
library(car)
boxCox(aov.out, family="yjPower", plotit = TRUE) #suggests taking a log of the data
w <- log(dat$Time)
w
new_data <- cbind(dat[,-9], w)
aov.out_1 <- aov(w ~ .*. , data =new_data)
plot(aov.out_1 <- aov(w ~ .*. , data =new_data))
summary(aov.out_1)

#identify outliers
cook <- cooks.distance(aov.out_1)
plot(cook,ylab="Cooks distances") 
s <- which(cook >= 0.25)
points(s,cook[s],col='red') 
  