/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#define DIM1 17
#define DIM2 39

void f (int **x, int **y)
{
  #pragma omp target map(iterator(i=0:DIM1), to: x[i][ :DIM2])
    ;

  #pragma omp target map(iterator(i=0:DIM1), to: x[i][ :DIM2], y[i][ :DIM2])
    ;

  #pragma omp target map(iterator(i=0:DIM1), to: x[i][ :DIM2] + 2) /* { dg-message "unsupported map expression" } */
    ;

  #pragma omp target map(iterator(i=0:DIM1), iterator(j=0:DIM2), to: x[i][j]) /* { dg-error "too many 'iterator' modifiers" } */
    ;

  #pragma omp target map(iterator(i=0:DIM1), to: (i % 2 == 0) ? x[i] : y[i]) /* { dg-message "unsupported map expression" } */
    ;
}
