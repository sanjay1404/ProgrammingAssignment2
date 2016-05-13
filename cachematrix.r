## Two functions that speed up the process of using the inverse of 
## a matrix by creating and using a cached copy.
## CacheMatrix takes a matrix as an input and outputs a list of
## functions that will set and retrieve variables (the input matrix and its
## inverse) when called (from cacheSolve).

Cachematrix <- function(x= matrix())
{
  inv <- NULL
 set <- function(y)
 {
  x <<- y
 inv <<- NULL
 }
 get <- function() x
 setInverse <- function(inverse)inv <<- inverse
 getInverse <- function() inv
 list(set = set,
      get = get,
   setInverse = setInverse,
   getInverse = getInverse)
}
## cacheSolve takes the list output from CacheMatrix and
## uses it to get the cached inverse of the matrix contained within, or
## to calculate it if the it has not already been set.

 cacheSolve <- function(x,...)
 {
 inv <- x$getInverse()
 if (!is.null(inv))
 {
 message("getting Cached Data")
 return(inv)
 }
 mat <- x$get()
 inv <- solve(mat,...)
 x$setInverse(inv)
 inv
 }
 
