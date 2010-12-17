/* PR c++/34607 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -std=gnu99" } */
void
foo ()
{
#pragma omp for
  for (int i =; i < 2; ++i)	/* { dg-error "expected expression before" } */
    ;
#pragma omp for
  for (T i = 54; i < 56; i++)	/* { dg-error "unknown type name" } */
    ;
  T j;				/* { dg-error "unknown type name" } */
#pragma omp for
  for (j = 1; j < 3; j++)
    ;
}
