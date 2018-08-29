/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O3 -mavx512f -mtune=knl" } */

#include "avx512f-check.h"

#define N 1024
int a[N], b[N], c[N], d[N];

__attribute__((noinline, noclone)) void
foo (float *__restrict p, float *__restrict q, float *__restrict r,
     int s1, int s2, int s3)
{
  int i;
  for (i = 0; i < N; i++)
    p[i] = q[a[i] * s1 + b[i] * s2 + s3] * r[c[i] * s1 + d[i] * s2 + s3];
}

static void
avx512f_test (void)
{
  int i;
  float e[N], f[N], g[N];
  for (i = 0; i < N; i++)
    {
      a[i] = (i * 7) & (N / 8 - 1);
      b[i] = (i * 13) & (N / 8 - 1);
      c[i] = (i * 23) & (N / 8 - 1);
      d[i] = (i * 5) & (N / 8 - 1);
      e[i] = 16.5 + i;
      f[i] = 127.5 - i;
    }
  foo (g, e, f, 3, 2, 4);
  for (i = 0; i < N; i++)
    if (g[i] != (float) ((20.5 + a[i] * 3 + b[i] * 2)
			 * (123.5 - c[i] * 3 - d[i] * 2)))
      abort ();
}
