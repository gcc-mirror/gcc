/* { dg-do compile } */
/* { dg-options "-fopenmp -O3 -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops in function" 4 "vect" } } */

int a[1024];

void
foo (void)
{
  #pragma omp simd if (0)
  for (int i = 0; i < 1024; ++i)
    a[i] = a[i] + 1;
}

void
bar (void)
{
  #pragma omp simd if (0) safelen (256) simdlen (8)
  for (int i = 0; i < 512; ++i)
    a[i] = a[i] + 1;
}

void
baz (void)
{
  #pragma omp simd safelen (256) simdlen (1)
  for (int i = 0; i < 512; ++i)
    a[i] = a[i] + 1;
}

void
qux (void)
{
  #pragma omp simd simdlen (1) if (1)
  for (int i = 0; i < 512; ++i)
    a[i] = a[i] + 1;
}
