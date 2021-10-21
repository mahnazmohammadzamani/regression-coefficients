regression <- function(X,y){
  betahat <- c(solve((t(X) %*% X))%*% (t(X) %*% y))
  return_obj <- list(Coefficients = betahat)
  return(return_obj)
}
data = iris
X <- model.matrix(Petal.Length~Species, data)
X
m <- data[all.vars(Petal.Length~Species)[1]]
m
y <- data.matrix(m)
y
regression(X,y)

#test
mod_object <- lm(Petal.Length~Species, data = iris)
print(mod_object)

O(m^3)