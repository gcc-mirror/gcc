/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_condition } } } */

int x;

void
foo (int *a)
{
  #pragma omp simd lastprivate (x)
  for (int i = 0; i < 1024; ++i)
    if (a[i])
      x = i;
}
