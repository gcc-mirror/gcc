/* PR c/47963 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (float n)
{
  int A[n][n];	/* { dg-error "has non-integer type" } */
#pragma omp parallel private(A)
  ;
}
