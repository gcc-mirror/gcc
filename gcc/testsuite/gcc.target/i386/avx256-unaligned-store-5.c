/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-store" } */

#include "avx-check.h"

#define N 8

float a[N] = { 24.43, 68.346, 43.35, 546.46, 46.79, 82.78, 82.7, 9.4 };
float b[N+3];
float c[N+3];

void
foo (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i+3] = a[i] * 2;
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
    c[i+3] = bar (a[i]);

  for (i = 0; i < N; i++)
    if (b[i+3] != c[i+3])
      abort ();
}
