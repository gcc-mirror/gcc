/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512i
__attribute__ ((noinline))
foo (int x)
{
  return _mm512_set_epi32 (x, x, x, x, x, x, x, x,
			   x, x, x, x, x, x, x, x);
}

static __m512i
__attribute__ ((noinline))
foo_r (int x)
{
  return _mm512_setr_epi32 (x, x, x, x, x, x, x, x,
			    x, x, x, x, x, x, x, x);
}

static void
avx512f_test (void)
{
  int i;
  int e = 0xabadbeef;
  int v[16];
  union512i_d res;

  for (i = 0; i < 16; i++)
    v[i] = e;

  res.x = foo (e);

  if (check_union512i_d (res, v))
    abort ();

  res.x = _mm512_setzero_si512 ();

  res.x = foo_r (e);

  if (check_union512i_d (res, v))
    abort ();
}
