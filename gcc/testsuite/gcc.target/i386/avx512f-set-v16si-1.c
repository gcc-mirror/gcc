/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512i
__attribute__ ((noinline))
foo (int *v)
{
  return _mm512_set_epi32 (v[15], v[14], v[13], v[12],
			   v[11], v[10], v[9], v[8],
			   v[7], v[6], v[5], v[4],
			   v[3], v[2], v[1], v[0]);
}

static __m512i
__attribute__ ((noinline))
foo_r (int *v)
{
  return _mm512_setr_epi32 (v[0], v[1], v[2], v[3],
			    v[4], v[5], v[6], v[7],
			    v[8], v[9], v[10], v[11],
			    v[12], v[13], v[14], v[15]);
}

static void
avx512f_test (void)
{
  int v[16] = { 19832468, 2134, 6576856, 6678,
		8723467, 54646, 234566, 12314,
		786784, 77575, 645245, 234555,
		9487733, 411244, 12344, 86533 };
  union512i_d res;

  res.x = foo (v);

  if (check_union512i_d (res, v))
    abort ();

  res.x = _mm512_setzero_si512 ();

  res.x = foo_r (v);

  if (check_union512i_d (res, v))
    abort ();
}
