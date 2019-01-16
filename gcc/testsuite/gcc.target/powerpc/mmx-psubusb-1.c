/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
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
  return _mm_subs_pu8 (s1, s2);
}

static void
TEST (void)
{
  __m64_union u, s1, s2;
  __m64_union e;
  int i, tmp;

  s1.as_m64 = _mm_set_pi8 (30, 2, 3, 4, 10, 20, 30, 90);
  s2.as_m64 = _mm_set_pi8 (88, 44, 3, 22, 11, 98, 76, 100);
  u.as_m64 = test (s1.as_m64, s2.as_m64);

  for (i = 0; i < 8; i++)
    {
      tmp = s1.as_char[i] - s2.as_char[i];

      if (tmp > 255)
        tmp = -1;
      if (tmp < 0)
        tmp = 0;

      e.as_char[i] = tmp;
    }

  if (u.as_m64 != e.as_m64)
    abort ();
}
