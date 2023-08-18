/* PR tree-optimization/88464 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f -mprefer-vector-width=512 -mtune=haswell -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "loop vectorized using 64 byte vectors" 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 4 "vect" } } */

__attribute__((noipa)) void
f1 (double * __restrict__ a, const double * __restrict__ b, const int * __restrict__ c, int n)
{
  int i;
#pragma GCC ivdep
  for (i = 0; i < n; ++i)
    if (a[i] > 10.0)
      a[i] = b[c[i]];
}

__attribute__((noipa)) void
f2 (double * __restrict__ a, const double * __restrict__ b, const long * __restrict__ c, int n)
{
  int i;
#pragma GCC ivdep
  for (i = 0; i < n; ++i)
    if (a[i] > 10.0)
      a[i] = b[c[i]];
}

__attribute__((noipa)) void
f3 (float * __restrict__ a, const float * __restrict__ b, const int * __restrict__ c, int n)
{
  int i;
#pragma GCC ivdep
  for (i = 0; i < n; ++i)
    if (a[i] > 10.0f)
      a[i] = b[c[i]];
}

__attribute__((noipa)) void
f4 (float * __restrict__ a, const float * __restrict__ b, const long * __restrict__ c, int n)
{
  int i;
#pragma GCC ivdep
  for (i = 0; i < n; ++i)
    if (a[i] > 10.0f)
      a[i] = b[c[i]];
}
