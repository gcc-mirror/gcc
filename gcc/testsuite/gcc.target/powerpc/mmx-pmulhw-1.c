/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
/* { dg-require-effective-target lp64 } */
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
  return _mm_mulhi_pi16 (s1, s2);
}

static void
TEST (void)
{
  __m64_union u, s1, s2;
  __m64_union e;
  int i, tmp;

  s1.as_m64 = _mm_set_pi16 (10, 2067, -3033, 90);
  s2.as_m64 = _mm_set_pi16 (11, 9834, 7444, -10222);
  u.as_m64 = test (s1.as_m64, s2.as_m64);

  for (i = 0; i < 4; i++)
    {
      tmp = s1.as_short[i] * s2.as_short[i];

      e.as_short[i] = (tmp & 0xffff0000)>>16;
    }

  if (u.as_m64 != e.as_m64)
    abort ();
}
