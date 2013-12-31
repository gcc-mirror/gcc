/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512
__attribute__ ((noinline))
foo (float *v)
{
  return _mm512_set_ps (v[15], v[14], v[13], v[12],
			v[11], v[10], v[9], v[8],
			v[7], v[6], v[5], v[4],
			v[3], v[2], v[1], v[0]);
}

static __m512
__attribute__ ((noinline))
foo_r (float *v)
{
  return _mm512_setr_ps (v[0], v[1], v[2], v[3],
			 v[4], v[5], v[6], v[7],
			 v[8], v[9], v[10], v[11],
			 v[12], v[13], v[14], v[15]);
}

static void
avx512f_test (void)
{
  float v[16] = { -3.3, 2.6, 1.48, 9.104, -23.9, 17, -13.48, 4,
		  69.78, 0.33, 81, 0.4, -8.9, -173.37, 0.8, 68 };
  union512 res;

  res.x = foo (v);

  if (check_union512 (res, v))
    abort ();

  res.x = _mm512_setzero_ps ();

  res.x = foo_r (v);

  if (check_union512 (res, v))
    abort ();
}
