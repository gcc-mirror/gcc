/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512
__attribute__ ((noinline))
foo (float x1, float x2, float x3, float x4,
     float x5, float x6, float x7, float x8,
     float x9, float x10, float x11, float x12,
     float x13, float x14, float x15, float x16)
{
  return _mm512_set_ps (x1, x2, x3, x4, x5, x6, x7, x8,
			x9, x10, x11, x12, x13, x14, x15, x16);
}

static __m512
__attribute__ ((noinline))
foo_r (float x1, float x2, float x3, float x4,
       float x5, float x6, float x7, float x8,
       float x9, float x10, float x11, float x12,
       float x13, float x14, float x15, float x16)
{
  return _mm512_setr_ps (x16, x15, x14, x13, x12, x11, x10, x9,
			 x8, x7, x6, x5, x4, x3, x2, x1);
}

static void
avx512f_test (void)
{
  float v[16] = { -3.3, 2.6, 1.48, 9.104, -23.9, 17, -13.48, 4,
		  69.78, 0.33, 81, 0.4, -8.9, -173.37, 0.8, 68 };
  union512 res;

  res.x = foo (v[15], v[14], v[13], v[12], v[11], v[10], v[9], v[8],
	       v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union512 (res, v))
    abort ();

  res.x = _mm512_setzero_ps ();

  res.x = foo_r (v[15], v[14], v[13], v[12], v[11], v[10], v[9], v[8],
		 v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union512 (res, v))
    abort ();
}
