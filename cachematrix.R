## R Programming Assignment 2
## Author: Mario Carlo Severo
## Date: 13-05-2022

## This R script serves as a requirement for the R Programming course in 
## Coursera ('Data Science Specialization Course of Johns Hopkins University'). 
## It contains two functions that performs the following tasks: 1) a function
## that creates a special "matrix" object that can cache its inverse and 2) a 
## function that computes the inverse of the special "matrix" returned 
## by first function. Note that the functions can only be applied to squared 
## matrices (i.e., matrices with 2 x 2 dimensions) as they are the only ones 
## that can possibly have inverses.


## Part 1: This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # creates empty matrix that will contain the matrix
  m <- NULL
  
  # sets the value of the matrix as its cached inverse value and gets that value  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  # creates a list that would contain the cached outputs above
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Part 2: This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above.If the inverse has already been 
## calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Part 3: Testing and running the script
## Creates a sample squared matrix
sample.mat <- matrix(data = 1:20, nrow = 2, ncol = 2)
sample.mat

## Makes cache of the matrix above using `makeCacheMatrix` function
cache.mat <- makeCacheMatrix(sample.mat)

## Returns the inverse of the cached matrix ('cache.mat') using the 
## 'solveCache` function
inv.mat <- cacheSolve(cache.mat)
inv.mat
