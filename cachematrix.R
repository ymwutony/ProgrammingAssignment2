## The function makeCacheMatrix is used for creating a special ## matrix that can cache its inverse.
## The function cacheSolve is used for computing the inverse
## of the matrix returned by the function makeCacheMatrix.

## The function makeCacheMatrix requires a square matrix as the  
## arguement and contains the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inv) {
                m <<- inv
        }
        getinv <- function() {
                m
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The function computes the inverse of the special matrix 
## created by makeCacheMatrix by using solve() function in R 
## package. Before the computation, it checks if the inverse has 
## already been computed. If so, it will get the inverse 
## directly from the cache and skip the computation. Otherwise, 
## the inverse will be computed and store in the cache for 
## further use.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) 
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
