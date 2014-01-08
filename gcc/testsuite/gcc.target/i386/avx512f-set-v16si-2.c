/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512i
__attribute__ ((noinline))
foo (int x1, int x2, int x3, int x4,
     int x5, int x6, int x7, int x8,
     int x9, int x10, int x11, int x12,
     int x13, int x14, int x15, int x16)
{
  return _mm512_set_epi32 (x1, x2, x3, x4, x5, x6, x7, x8,
			   x9, x10, x11, x12, x13, x14, x15, x16);
}

static __m512i
__attribute__ ((noinline))
foo_r (int x1, int x2, int x3, int x4,
       int x5, int x6, int x7, int x8,
       int x9, int x10, int x11, int x12,
       int x13, int x14, int x15, int x16)
{
  return _mm512_setr_epi32 (x16, x15, x14, x13, x12, x11, x10, x9,
			    x8, x7, x6, x5, x4, x3, x2, x1);
}

static void
avx512f_test (void)
{
  int v[16] = { -3, -453, 2, -231, 1, -111, 9, -145,
		23, 671, -173, 166, -13, 714, 69, 123 };
  union512i_d res;

  res.x = foo (v[15], v[14], v[13], v[12], v[11], v[10], v[9], v[8],
	       v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union512i_d (res, v))
     abort ();

  res.x = _mm512_setzero_si512 ();

  res.x = foo_r (v[15], v[14], v[13], v[12], v[11], v[10], v[9], v[8],
		 v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union512i_d (res, v))
     abort ();
}
