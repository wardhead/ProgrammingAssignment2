## The first function in this file will create a special matrix object that can store 
## and retrieve a matrix and its inverse.  The second function will solve and cache the
## inverse matrix the first time it is called.  After that, it will simply returned the
## cached inverse.  (It is assumed that the matrix has an inverse.)
##
## To use:
##     x <- makeCacheMatrix(<original matrix>)
##     x$get()        ## returns <original matrix>
##     cacheSolve(x)  ## solves, caches, and returns the inverse of <original matrix>
##     cacheSolve(x)  ## returns the cached inverse of <original matrix>
##
## Note: A cleaner solution would be to automatically compute the inverse when the matrix 
##       is stored and eliminate the cacheSolve function entirely, but this wasn't the
##       assignment.
##
##
## makeCacheMatrix stores a matrix and creates an object with the following methods:
##   get: returns the matrix
##   cacheinverse: caches (what we believe to be) the inverse of the matrix
##   getinverse: returns the cached inverse  
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  cacheinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv      ## returns NULL if no inverse has been cached
  list(get=get, cacheinverse=cacheinverse, getinverse=getinverse)
}

## This function returns the inverse of the matrix stored in an object created by 
## makeCacheMatrix.  If this is the first time the inverse has been returned, it will be
## solved and cached.
cacheSolve <- function(x, ...) {
  if(is.null(x$getinverse())) 
     x$cacheinverse(solve(x$get()))
  x$getinverse()
}
