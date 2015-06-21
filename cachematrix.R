## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## In this R code I wrote a pair of functions that cache and compute 
## the inverse of a matrix.

## The "makeCacheMatrix" function creates a special object that 
## cache its inverse.


makeCacheMatrix <- function(xmx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    xmx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(xmx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function

## The second function computes the inverse of the special
## object returned by "makeCacheMatrix". If the inverse has
## already been calculated the "cacheSolve" should retrieve 
## the inverse from the cache.

cacheSolve <- function(xmx, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- xmx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached...")
    return(inverse)
  }
  data <- xmx$get()
  invserse <- solve(data, ...)
  xmx$setinv(inverse)
  return(inverse)
}
