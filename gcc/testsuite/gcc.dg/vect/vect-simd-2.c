/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fopenmp-simd" } */

#define N 1024
int a[N];
int bar (void);

void
foo (void)
{
  #pragma omp simd if (bar ())
  for (int i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "created versioning for simd if condition check" 1 "vect" } } */
