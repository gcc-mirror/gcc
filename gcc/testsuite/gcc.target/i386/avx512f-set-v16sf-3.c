/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512
__attribute__ ((noinline))
foo (float x)
{
  return _mm512_set_ps (x, x, x, x, x, x, x, x,
			x, x, x, x, x, x, x, x);
}

static __m512
__attribute__ ((noinline))
foo_r (float x)
{
  return _mm512_setr_ps (x, x, x, x, x, x, x, x,
			 x, x, x, x, x, x, x, x);
}

static void
avx512f_test (void)
{
  int i;
  float e = 34.5;
  float v[16];
  union512 res;

  for (i = 0; i < 16; i++)
    v[i] = e;

  res.x = foo (e);

  if (check_union512 (res, v))
    abort ();

  res.x = _mm512_setzero_ps ();

  res.x = foo_r (e);

  if (check_union512 (res, v))
    abort ();
}
