/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fopenmp-simd" } */

#define N 1024
int a[N];

void
foo (void)
{
  int c = 1;
  #pragma omp simd if (c)
  for (int i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "created versioning for simd if condition check" "vect" } } */
