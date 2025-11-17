/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#define DIM1 17
#define DIM2 39

void f (int **x, float **y)
{
  #pragma omp target update to (iterator(i=0:DIM1): x[i][ :DIM2])

  #pragma omp target update to (iterator(i=0:DIM1): x[i][ :DIM2], y[i][ :DIM2])

  #pragma omp target update to (iterator(i=0:DIM1), present: x[i][ :DIM2])

  #pragma omp target update to (iterator(i=0:DIM1), iterator(j=0:DIM2): x[i][j]) /* { dg-error "too many 'iterator' modifiers" } */
  /* { dg-error "'#pragma omp target update' must contain at least one 'from' or 'to' clauses" "" { target *-*-* } .-1 } */

  #pragma omp target update from (iterator(i=0:DIM1), something: x[i][j]) /* { dg-error "'from' clause with modifier other than 'iterator' or 'present'" } */
  /* { dg-error "expected '\\)' before 'something'" "" { target c } .-1 } */
  /* { dg-error "'#pragma omp target update' must contain at least one 'from' or 'to' clauses" "" { target *-*-* } .-2 } */
}
