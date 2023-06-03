############################################
########## ADVANCED ECONOMETRICS I #########
############################################
####      The code is prepared  by      ####
####       Yuri Trifonov, NRU HSE       ####
############################################
##########  CLASS 5. Intro to R ############
############################################

### Problem 1. The analysis of apartments
############################################

### POINT (a) 
# First, we need to install and upload all of the 
# important libraries

## Install libraries (you only need to do it once!)
## Uncomment and compile each row if you do not have
## these libraries yet
##############################
#install.packages('rcompanion', dependencies = TRUE)
#install.packages('readxl', dependencies = TRUE)
#install.packages('psych', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('car', dependencies = TRUE)
#install.packages('lmtest', dependencies = TRUE)
##############################
# Disable scientific notation
options(scipen = 999)
# Upload the packages to current R session
# (you should do it every time you restart R)
##############################
library('readxl')     # for reading excel files
library('rcompanion') # for plotting normal curve over histograms easily
library('psych')
library('ggplot2')    # for plotting beautiful graphs
library('car')
library('lmtest')
##############################
# Now we need to load the data from the .xlsx file
# specify the correct path to the data on your computer
df <- as.data.frame(read_excel("/Users/jurytrifonov/Desktop/Teaching/Class 5/data.xlsx"))      # load the data
View(df)                                         # we can take a look at the data
df$price                                         # look at specific variables
df$brick                                         # look at specific variables

### POINT (b) 
# Let's have a look at the histogram of price 
# and compare it with the normal distribution
plotNormalHistogram(df$price)   # for price

hist(df$price)

plotNormalHistogram(df$totsp)  # for total space

# Now let's compute the logarithm of this variable
# and add it to our data
df$ln_price <- log(df$price)

# Let's look at the histogram of this new variable
plotNormalHistogram(df$ln_price)

### POINT (c) 
# Now let's create the variable,
# representing the price per sq. meter.
# and add it to our data
df$price_msq <- df$price / df$totsp

# Also we can create the log variable in case we need it
df$ln_price_msq <- log(df$price_msq)

# Look at the histogram of price_msq
plotNormalHistogram(df$price_msq)

### POINT (d) 
# Now we will provide descriptive statistics for our data
summary(df)
describe(df[2:13])

### POINT (e) 
# For continues variables we usually
# provide scatter plots to analyze the dependence
plot(df$metrdist, df$price_msq)    # distance from metro stations and price_msq
abline(lm(df$price_msq ~ df$metrdist), col = "red")
# For discrete variables we usually 
# provide simple bar plots
df_bar <- aggregate(df$price_msq, list(df$brick), FUN=mean)
ggplot(df_bar, aes(x = factor(Group.1), y = x)) + 
  geom_bar(stat = "identity") + 
  labs(x="Brick (Yes/No)", y="Mean price per sq.meter")

boxplot(df$price_msq~df$brick, main="Prices of appartments", 
        xlab="Brick (Yes/No)", ylab="Price per sq.m")

### POINT (f) 
# To analyze correlations between variables let's 
# estimate the correlation matrix
cor(df)       # for all variables
cor(df[8:14]) # for specific variables

### POINT (g)
# Now let's compare the mean of price_msq by floor
# To do this we will conduct a simple t-test for
# comparing the means between two samples

t.test(df$price_msq~df$floor) # for floor

t.test(df$price_msq~df$walk)  # for walk


### POINT (h)
# Now we will estimate different models via the OLS method

model_1 <- lm(price_msq ~ dist, data = df)
summary(model_1)
model_2 <- lm(price_msq ~ dist + kitsp + walk, data = df)
summary(model_2)
model_3 <- lm(ln_price_msq ~ dist + kitsp + livesp, data = df)
summary(model_3)
vif(model_3) # calculate VIFs

# If we want to perform F-test for testing joint significance
linearHypothesis(model_2, c("kitsp=0", "walk=0"))
### POINT (j)
# Finally let's compare the effect of distance on price for
# appartments that are located in a walking distance
# and those that are not
model_4 <- lm(df$price_msq[df$walk == 1] ~ df$dist[df$walk ==1]) # walking distance
model_5 <- lm(df$price_msq[df$walk == 0] ~ df$dist[df$walk ==0]) # not in a walking distance
summary(model_4) 
summary(model_5)

### Simple example of creating a function in R
### for solving a quadratic equation
test_func <- function(a,b,c)      # create a function that takes as input variables a,b,c
{
  D <- b ^ 2 - 4 * a * c          # calculate the discriminant
  
  x_1 <- (-b + sqrt(D)) / (2 * a) # find root 1
  x_2 <- (-b - sqrt(D)) / (2 * a) # find root 2
  
  result <- c(x_1, x_2)           # create the vector, combining both roots
  
  return(result)                  # return this vector (output of the function)
}

test_func(a = 1, b = 3, c = 2)  # apply the function with specific values for a,b,c
                                # and look at the result

################# NEXT CLASS #####################
model_A <- lm(price_msq ~ dist + livesp + brick, data = df)
model_B <- lm(price_msq ~ dist + kitsp + floor + metrdist + walk, data = df)
summary(model_A)
summary(model_B)

# Let's perform non nested F test to check if one model is nested in another
## For model A
model_add <- lm(price_msq ~ dist + livesp + brick + kitsp + floor + metrdist + walk, data = df)
summary(model_add)
## For model A
linearHypothesis(model_add, c("kitsp=0", 'floor=0', "metrdist=0", 'walk=0'))
## For model B
linearHypothesis(model_add, c("brick=0", "livesp=0"))
#INTERPRETATION: Both models are not nested in each other!

# Perform a non-nested J-test
jtest(model_A, model_B)
#INTERPRETATION: Both models are not nested in each other!





