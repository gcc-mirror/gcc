/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__((noinline))
foo (short x)
{
  return _mm256_set_epi16 (x, x, x, x, x, x, x, x,
			   x, x, x, x, x, x, x, x);
}

static void
avx_test (void)
{
  short e = 345;
  short v[16];
  union256i_w u;
  int i;

  for (i = 0; i < ARRAY_SIZE (v); i++)
    v[i] = e;
  u.x = foo (e);
  if (check_union256i_w (u, v))
    abort ();
}
