/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__((noinline))
foo (long long x)
{
  return _mm256_set_epi64x (x, x, x, x);
}

static void
avx_test (void)
{
  long long e = 0xfed178ab134badf1LL;
  long long v[4];
  union256i_q u;
  int i;

  for (i = 0; i < ARRAY_SIZE (v); i++)
    v[i] = e;
  u.x = foo (e);
  if (check_union256i_q (u, v))
    abort ();
}
