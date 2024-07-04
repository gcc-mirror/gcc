/* { dg-do run } */
/* { dg-options "-O3 -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1
#ifndef CHECK_H
#define CHECK_H "mmx-check.h"
#endif

#ifndef TEST
#define TEST mmx_test
#endif

#include CHECK_H

#include <mmintrin.h>

static __m64
__attribute__((noinline, unused))
test (__m64 s1, __m64 s2)
{
  return _mm_madd_pi16 (s1, s2);
}

static void
TEST (void)
{
  __m64_union u, s1, s2;
  __m64_union e;
  int i;

  s1.as_m64 = _mm_set_pi16 (2134, 3334, 1234, 6354);
  s2.as_m64 = _mm_set_pi16 (1, 3, 4, 5);
  u.as_m64 = test (s1.as_m64, s2.as_m64);

  for (i = 0; i < 2; i++)
    e.as_int[i] = (s1.as_short[i * 2] * s2.as_short[i * 2])
	+ (s1.as_short[(i * 2) + 1] * s2.as_short[(i * 2) + 1]);

  if (u.as_m64 != e.as_m64)
    abort ();
}
