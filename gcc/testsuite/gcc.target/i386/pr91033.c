/* PR tree-optimization/91033 */
/* { dg-do compile { target pthread } } */
/* { dg-options "-march=knl -O2 -fopenmp-simd -ftree-parallelize-loops=2" } */

#define N 1024
int a[N];

void
foo (void)
{
  int i;
  #pragma omp simd simdlen (4)
  for (i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}
