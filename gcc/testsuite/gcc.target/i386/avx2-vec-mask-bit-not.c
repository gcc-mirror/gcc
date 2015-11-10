/* { dg-do compile } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-mavx2 -O3 -fopenmp-simd -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

#define N 1024

int a[N], b[N], c[N], d[N], e[N];

void
test (void)
{
  int i;
  #pragma omp simd
  for (i = 0; i < N; i++)
    if (!(a[i] > b[i] && c[i] < d[i]))
      e[i] = 0;
}
