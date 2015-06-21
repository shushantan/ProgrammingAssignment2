## This is to cache and compute the inverse of a matrix
## usage:
## matrix <- matrix()
## x <- makeCacheMatrix(matrix)
## cacheSolve(x)

## makeCacheMatrix this is a function which stores 
## 4 function set , get ,setInv,getInv of a Matrix object
## it will reset the inverse if changes are made to the matrix object
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInv <- function(inv) inverse <<- inv
        getInv <- function() inverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve function computes the inverse of matrix object
## if the inverse has already been calculated ( and the matrix has not changed)
## it will retrieve inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
}
