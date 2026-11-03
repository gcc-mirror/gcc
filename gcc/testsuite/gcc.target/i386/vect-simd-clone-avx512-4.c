/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-options "-O2 -fopenmp-simd -mavx512bw -mprefer-vector-width=512 --param vect-partial-vector-usage=2" } */

#include "avx512bw-check.h"

#pragma omp declare simd simdlen(32) inbranch
int __attribute__((noinline)) baz (int x) { return 1000 / x; }

short d[1024];
int b[1024];

void __attribute__((noipa))
foo (int n)
{
  for (int i = 0; i < n; ++i)
    {
      d[i] = (short) (d[i] + 1);
      if ((i & 1) == 0)
	b[i] = baz (b[i]);
    }
}

static void
avx512bw_test (void)
{
  for (int i = 0; i < 1024; i++)
    {
      b[i] = i + 1;
      d[i] = 0;
    }
  foo (17);
  for (int i = 0; i < 64; i++)
    if (b[i] != ((i < 17 && (i & 1) == 0) ? 1000 / (i + 1) : i + 1)
	|| d[i] != (i < 17))
      abort ();
}
