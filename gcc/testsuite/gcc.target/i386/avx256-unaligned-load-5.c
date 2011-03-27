/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-load" } */

#include "avx-check.h"

#define N 8

float a[N+3] = { -1, -1, -1, 24.43, 68.346, 43.35,
		 546.46, 46.79, 82.78, 82.7, 9.4 };
float b[N];
float c[N];

void
foo (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i] = a[i+3] * 2;
}

__attribute__ ((noinline))
float
bar (float x)
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
