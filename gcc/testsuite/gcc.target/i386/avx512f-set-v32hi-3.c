/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx512f-check.h"

static __m512i
__attribute__((noinline))
foo (short x)
{
  return _mm512_set_epi16 (x, x, x, x, x, x, x, x,
			   x, x, x, x, x, x, x, x,
			   x, x, x, x, x, x, x, x,
			   x, x, x, x, x, x, x, x);
}

static void
avx512f_test (void)
{
  short e = -45;
  short v[32];
  union512i_w u;
  int i;

  for (i = 0; i < ARRAY_SIZE (v); i++)
    v[i] = e;
  u.x = foo (e);
  if (check_union512i_w (u, v))
    abort ();
}
