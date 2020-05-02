## Put comments here that give an overall description of what your
## functions do

## This function creates a "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           # Init inv with NULL
    set <- function(y) {  # define set function
        x <<- y           # Assign matrix in parent environment
        inv <<- NULL      # Reset inv to NULL
    }
    get <- function() x   # Define a getter to return the matrix
    setinverse <- function(inverse) inv <<- inverse     # Define the getter of the matrix in the parent environment
    getinverse <- function() inv                        # Define the getter of the inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # Define the functions
}


## This function calculates the inverse of a matrix and stores in the matrix cache defined above

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Get the inverse of the matrix
    if(!is.null(inv)) {    #  Check if inv is not NULL in order to return it
        return(inv)
    }
    data <- x$get()          # Set the matrix if not already done
    inv <- solve(data, ...)  # Call solve function
    x$setinverse(inv)        # Set the inverse of matrix
    inv                      # Return the matrix and its cached inverse
}
