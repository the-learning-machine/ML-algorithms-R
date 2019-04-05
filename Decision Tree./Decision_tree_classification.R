# Description of the problem: 
# Task is to predict whether the car is four-door or two-door based on price and horsepower
# Importing the dataset
dataset = read.csv('https://ibm.box.com/shared/static/q6iiqb1pd7wo8r3q28jvgsrprzezjqk3.csv',stringsAsFactors = FALSE)
new_dataset = dataset[c(22,26,6)]

# Initializing "four" and "two" as 1 and 0
for (i in 1:dim(new_dataset)[1]){
  if (new_dataset$num.of.doors[i] == "four"){
    new_dataset$num.of.doors[i] <- 1
  }
  else {
    new_dataset$num.of.doors[i] <- 0
  }
}

# Encoding the target feature as factor. It it is necessary for the model to understand that "0" and "1" are not numbers in our case.
new_dataset$num.of.doors = factor(new_dataset$num.of.doors, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set.
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(new_dataset$num.of.doors, SplitRatio = 0.75)
training_set = subset(new_dataset, split == TRUE)
test_set = subset(new_dataset, split == FALSE)

# Feature Scaling
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

# Fitting classifiere to the Training Set
# Here the part that is necessary to tune is "minsplit" in control group, which is the minimum number of observations that must exist in a node in order for a split to be attempted.
# We advise you to try to play with the number you put in for the "minsplit" value. Then run the code again in order to see the difference on the graph
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = num.of.doors ~ .,
                   data = training_set,
                   control = rpart.control(minsplit = 20))

# Predicting the Test set results
pred_val = predict(classifier, newdata = test_set[-3], type = 'class')

# Making the Confusion Matrix. Type in "cm" in command panel after running this code, you'll see the matrix. 
# All correct predictions are located in the diagonal of the matrix.
cm = table(test_set[, 3], pred_val)

# Visualising the Training set results
# install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('horsepower', 'price')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree Classification (Training set)',
     xlab = 'Horsepower', ylab = 'Price',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('horsepower', 'price')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree (Test set)',
     xlab = 'Horsepower', ylab = 'Price',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# PLotting Decision Tree
plot(classifier)
text(classifier)
