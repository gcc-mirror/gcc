/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__((noinline))
foo (long long x, int i)
{
  switch (i)
    {
    case 3:
      return _mm256_set_epi64x (x, 1, 1, 1);
    case 2:
      return _mm256_set_epi64x (1, x, 1, 1);
    case 1:
      return _mm256_set_epi64x (1, 1, x, 1);
    case 0:
      return _mm256_set_epi64x (1, 1, 1, x);
    default:
      abort ();
    }
}

static void
avx_test (void)
{
  long long e = 0xabadbeef01234567LL;
  long long v[4];
  union256i_q u;
  int i, j;

  for (i = 0; i < ARRAY_SIZE (v); i++)
    {
      for (j = 0; j < ARRAY_SIZE (v); j++)
	v[j] = 1;
      v[i] = e;
      u.x = foo (e, i);
      if (check_union256i_q (u, v))
	abort ();
    }
}
