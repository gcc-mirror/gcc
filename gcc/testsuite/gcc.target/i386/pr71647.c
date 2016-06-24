/* PR tree-optimization/71647 */
/* { dg-do compile } */
/* { dg-options "-O3 -fopenmp-simd -mavx -mno-avx512f -fdump-tree-vect-details" } */

void
foo (double *a, double *b)
{
  int i;
#pragma omp simd aligned(a,b:4*sizeof(double))
  for (i = 0; i < 32768; i++)
    a[i] += b[i];
}

void
bar (double *a, double *b)
{
  int i;
#pragma omp simd aligned(a,b:32)
  for (i = 0; i < 32768; i++)
    a[i] += b[i];
}

void
baz (double *a, double *b)
{
  int i;
#pragma omp simd aligned(a,b:32L)
  for (i = 0; i < 32768; i++)
    a[i] += b[i];
}

/* { dg-final { scan-tree-dump-not "Alignment of access forced using peeling" "vect" } } */
