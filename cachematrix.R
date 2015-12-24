## This package contains two functions; makeCacheMatrix which
## creates a special "matrix" object that can cache its inverse,
## and cacheSolve which should compute the inverse of the special
## "matrix," by either computing and storing it, or if that has
## been done, retrieving the cached version.

## Create a special "matrix" object that can cache its inverse.
## It's really a list of functions.  No need to check that the
## matrix is square and invertible - we are told to assume that,
## and the error on solve will bubble up anyways. x is the
## matrix you want to invert.
makeCacheMatrix <- function(x = matrix()) {
  ## Clear m
  m <- NULL
  ## Make a function to set the cache to local m and x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Make a function to get the original matrix
  get <- function() x
  ## Make a function that caches the solved matrix
  setSolve <- function(solve) m <<- solve
  ## Make a function that gets the solved matrix from the cache
  getSolve <- function() m
  ## Pass back the "matrix" object which is a list of functions 
  ## which can get the original matrix, the pair of them, 
  ## and the solved values.
  list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Compute the inverse of the special "matrix," by either 
## computing and storing it, or if that has been done, 
## retrieving the cached version.  x is a "special"
## matrix, which is a list of functions: set, get, 
## setSolve, and getSolve, which has a value if it has 
## been cached.
cacheSolve <- function(x, ...) {
  ## Look for the getSolve value, if it has one, it's a
  ## special matrix and we can return that getSolve value.
  m <- x$getSolve()
  if(!is.null(m)) {
    message("Getting cached data.")
    return(m)
  }
  
  ## We haven't returned, so get the original data
  data <- x$get()
  
  ## Solve it
  m <- solve(data, ...)
  
  ## Store it back in the cache
  x$setSolve(m)
  
  ## Return the computation we just did.
  m
}