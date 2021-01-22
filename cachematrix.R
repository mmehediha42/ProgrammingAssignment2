## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function to create a special type of matrix ( a cacheable matrix )

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse_matrix <<- solve
        getinverse <- function() inverse_matrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## Function to calculate the inverse of the special "matrix" created with the above function (makeCacheMatrix). 
# It first checks to see if the inverse of the matrix is already there. If so, it gets the inverse of the matrix from 
#the cache and skips the computation. Otherwise, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if (!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}