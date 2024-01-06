## Write a function to create a matrix, solve it, and cache in the inverse to avoid unnecessary recomputation

## This function initializes a matrix x as an argument with a default value of an empty matrix.#
## It then intializes sv as a variable to store the resulte of solving the inverse matrix.#
## it sets x to equal y in the parent environment and sets sv to the parent environment so it can be found using lexical scoping#
# Get finds the function of x which should be in the global environment##
## It then creates a function that takes the inverse as an argument and sets sv to inverse in the global environment##
## Getinv looks for the function of the inverse in the global environment##
## list sets these functions as a named list ##


makeCacheMatrix <- function(x = matrix()) {
        sv <- NULL
        set <- function(y) {
                x <<- y
                sv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) sv <<- inverse
        getinv <- function() sv
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}

## This defines a function that takes the argument x with the option to pass additional arguments throught ...##
## sv is set to the inverse of x found in the global environment (cached)
## if sv is not null then it gets the inverse from the cache and returns it without recomputing the inverse ##
## if not already computed it then sets data to the value (a matrix) extracted from the function get ##
##  it then solves the inverse of data and sets it to sv ##
## it then caches sv so it can be retrieved without recomputation in the future ##
cacheSolve <- function(x, ...) {
        sv <- x$getinv()
        if(!is.null(sv)) {
                message("getting cached data")
                return(sv)
        }
        data <- x$get()
        sv <- solve(data, ...)
        x$setinv(sv)
        sv

        ## Return a matrix that is the inverse of 'x'
        
}
