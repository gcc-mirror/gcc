/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-load" } */

#include "avx-check.h"

#define N 4

double a[N+3] = { -1, -1, -1, 24.43, 68.346, 43.35, 546.46 };
double b[N];
double c[N];

void
foo (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i] = a[i+3] * 2;
}

__attribute__ ((noinline))
double
bar (double x)
{
  return x * 2;
}

void
avx_test (void)
{
  int i;

  foo ();

  for (i = 0; i < N; i++)
    c[i] = bar (a[i+3]);

  for (i = 0; i < N; i++)
    if (b[i] != c[i])
      abort ();
}
