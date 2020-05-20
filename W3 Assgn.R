## Defining makeCacheMatrix 
# Creating a function that takes 'input matrix' as input
# and stores the inverse of matrix in cache 

makeCacheMatrix <- function(input_matrix=matrix()){
  matr <- NULL
  set <- function(y){
    input_matrix <<- y
    matr <<- NULL
  }
  get <- function() input_matrix
  solveMat <- function(solve) matr<<- solve
  getMat <- function() matr
  list(set = set, get = get,
       solveMat = solveMat,
       getMat = getMat)
}

#Another function that would take input as the list output of earlier function "makeCacheMatrix"
#extract cached matrix (defines in earlier function)

cacheSolve <- function(input_matix, ...){
  matr <- input_matix$getMat()
  if(!is.null(matr)){
    message("Cached Inverse Available")
    return(matr)
  }
  data <-input_matix$get()
  matr<-solve(data)
  input_matix$solveMat(matr)
  matr
}

mat_one <- matrix(floor(rnorm(16, 5, 2)), 4,4)
mat_one
solve(mat_one)

mat_one1 <- makeCacheMatrix(mat_one)

cacheSolve(mat_one1)
