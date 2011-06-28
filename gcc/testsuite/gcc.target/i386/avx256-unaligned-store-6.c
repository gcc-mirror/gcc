/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-store" } */

#include "avx-check.h"

#define N 4

double a[N] = { 24.43, 68.346, 43.35, 546.46 };
double b[N+3];
double c[N+3];

void
foo (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i+3] = a[i] * 2;
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
    c[i+3] = bar (a[i]);

  for (i = 0; i < N; i++)
    if (b[i+3] != c[i+3])
      abort ();
}
