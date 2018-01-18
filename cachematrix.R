## Below, under "SOLUTIONS", is a pair of functions that cache the inverse of a 
## matrix.

## Given the 1st function, makeVector, that creates a special "vector" 
## which is really a list containing a function to:

## 1. sets the value of the vector
## 2. gets the value of the vector
## 3. sets the value of the mean
## 4. gets the value of the mean 

## makevector:

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## And given the 2nd function, cachemean, that calculates the mean of the special
## "vector" created with the above function. However, it first checks to see if 
## the mean has already been calculated. If so, it gets the mean from the cache 
## and skips the computation. Otherwise, it calculates the mean of the data and 
## sets the value of the mean in the cache via the setmean function.

## cachemean:

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## SOLUTIONS:

## This function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above:

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

