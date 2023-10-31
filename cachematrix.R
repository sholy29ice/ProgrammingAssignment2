## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function() {
    # Initialize the matrix and cache
    matrix <- NULL
    cachedInverse <- NULL
    
    # Set a function to set the matrix
    set <- function(x) {
        matrix <<- x
        cachedInverse <<- NULL
    }
    
    # Set a function to get the matrix
    get <- function() matrix
    
    # Set a function to compute and cache the inverse
    setCache <- function() {
        if (is.null(matrix)) {
            message("Matrix is empty. Please set the matrix first.")
            return(NULL)
        }
        if (is.null(cachedInverse)) {
            message("Calculating and caching inverse.")
            cachedInverse <<- solve(matrix)
        } else {
            message("Retrieving cached inverse.")
        }
        return(cachedInverse)
    }
    
    # Return a list of functions
    list(set = set, get = get, setCache = setCache)
}

## Write a short comment describing this function

# Compute the cached inverse
cacheSolve <- function(cacheMatrix) {
    cacheMatrix$setCache()
}