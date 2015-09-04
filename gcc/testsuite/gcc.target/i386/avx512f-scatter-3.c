/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O3 -mavx512f" } */

#define AVX512F

#include "avx512f-check.h"

#define N 1024
int a[N], b[N];

__attribute__((noinline, noclone)) void
foo (float *__restrict p, float *__restrict q,
     int s1, int s2, int s3)
{
  int i;
  for (i = 0; i < (N / 8); i++)
    p[a[i] * s1 + b[i] * s2 + s3] = q[i];
}

static void
avx512f_test (void)
{
  int i;
  float c[N], d[N];
  for (i = 0; i < N; i++)
    {
      a[i] = (i * 7) & (N / 8 - 1);
      b[i] = (i * 13) & (N / 8 - 1);
      c[i] = 179.13 + i;
    }
  foo (d, c, 3, 2, 4);
  for (i = 0; i < (N / 8); i++)
    if (d[a[i] * 3 + b[i] * 2 + 4] != (float) (179.13 + i))
      abort ();
}
