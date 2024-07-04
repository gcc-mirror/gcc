/* { dg-do run } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-O2 -fopenmp-simd -mavx512vl" } */

#include "avx512vl-check.h"

#ifndef SIMDLEN
#define SIMDLEN 4
#endif

int x[1024];

#pragma omp declare simd simdlen(SIMDLEN)
__attribute__((noinline)) int
foo (int a, int b)
{
  return a + b;
}

void __attribute__((noipa))
bar (void)
{
#pragma omp simd
  for (int i = 0; i < 1024; i++)
    if (x[i] < 20)
      x[i] = foo (x[i], x[i]);
}

void avx512vl_test ()
{
  int i;
#pragma GCC novector
  for (i = 0; i < 1024; i++)
    x[i] = i;

  bar ();

#pragma GCC novector
  for (i = 0; i < 1024; i++)
    if ((i < 20 && x[i] != i + i)
	|| (i >= 20 && x[i] != i))
      abort ();
}
