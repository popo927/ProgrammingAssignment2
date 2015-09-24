# makeCacheMatrix creates a list containing a function to 
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
# which is just similar to the example for list.

makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setIv <- function(Iv) i <<- Iv
    getIv <- function() i
    list(set = set, get = get, setIv = setIv, getIv = getIv)
}   

# The cacheSolve function returns the inverse of the matrix. It checks if
# the inverse has already been computed. If yes, then it gets the result from cache.
# Else, it computes the inverse, sets the value in the cache.
# This function assumes that the matrix is always invertible so I didn't do other 
# handling for those not invertible.

cacheSolve <- function(x, ...){
    i <- x$getIv()
    if(!is.null(i)) {
        message("getting cached data~")
        return(i)
    }
    message("cached data not existed")
    data <- x$get()
    i <- solve(data, ...)
    x$setIv(i)
    i
}
