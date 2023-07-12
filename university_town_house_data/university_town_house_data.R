# A real estate economist collects information on 1000 house price sales from two similar
# neighborhoods, one called “University Town” bordering a large state university, and another a
# neighborhood about three miles from the university. He specifies the following regression
# equation:
#   
#   y = β0 + β1x1 + δ2D2 + γ(D2 ∗ x1
#                            
#   ) + β3 x3 + δ4D4 + δ5D5 + ε
# 
# where
# y = house prices in $1000
# x1 = the number of hundreds of square feet of living area
# D2 ={
#   1 house near university
#   0 otherwise
#   x3 = age of the house (in years)
#   D4 ={
#     1 house has pool
#     0 otherwise
#     D5 ={
#       1 if fireplace is present
#       0 otherwise
#       Discuss the effect of these variables on house prices




##################################### SOLUTION ##################

# Read the dataset
df <- read.table("C:/Users/Asus/Downloads/housing.txt", header = TRUE)
head(df, n=10)
# Fit the regression model
summary(df)

hist(df)
####################################  Histogram  ######################
# Create a new plot window
par(mfrow = c(2, 3))  # Adjust the numbers to arrange the histograms in the desired layout

# Iterate through each variable (assuming your dataset is named 'df')
for (col in colnames(df)) {
  hist(df[[col]], main = col, xlab = col, ylab = "Frequency")
}


################################### Boxplot  ##########################

# Create a new plot window
par(mfrow = c(2, 3))  # Adjust the numbers to arrange the boxplots in the desired layout

# Iterate through each variable (assuming your dataset is named 'df')
for (col in colnames(df)) {
  boxplot(df[[col]], main = col, xlab = col, ylab = "Value")
}


##################################  correlation matrix  ##################
# Compute the correlation matrix for all variables (assuming your dataset is named 'df')
cor_matrix <- cor(df)

# Print the correlation matrix
print(cor_matrix)

pairs(df)


################################# relation between price and all other variables  #########

# Create a new plot window
par(mfrow = c(2, 3))  # Adjust the numbers to arrange the plots in the desired layout

# Iterate through each variable (excluding the house price variable)
for (col in colnames(df)[-which(colnames(df) == "P")]) {
  plot(df[[col]], df$P, main = paste("Scatter plot:", col), xlab = col, ylab = "House Price")
}



##################################### Fit the regression model ############


# Print the model summary
#summary(model)

library(car)
ml1=lm(P~S+Ut+I(S*Ut)+A+Pol+Fp+I(Pol*Fp),data=df)
ml1
library(car)
vif_values<-vif(ml1)
vif_values
plot(ml1)
ssrf=sum(resid(ml1)^2)
ssrf
ml2=lm(P~S+A,data=df)
summary(ml2)
ssrf2=sum(resid(ml2)^2)
ssrf2
ml3=lm(P~S+Ut+I(S*Ut)+A+Pol+Fp,data=df)
summary(ml3)
