/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

#define N 1024

void
foo (int *x, int *y, int *z, int a)
{
  int i;
  #pragma omp simd if (simd: a > 2) aligned (x, y, z : 16)
  for (i = 0; i < N; i++)
    x[i] = y[i] + z[i];
}
