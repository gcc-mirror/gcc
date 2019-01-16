/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_psadbw_1
#endif

#include <xmmintrin.h>

static __m64
__attribute__((noinline, unused))
test (__m64 s1, __m64 s2)
{
  return _mm_sad_pu8 (s1, s2);
}

static void
TEST (void)
{
  __m64_union u, e, s1, s2;
  unsigned char tmp[8];
  int i;

  e.as_m64 = 0;
  s1.as_m64 = _mm_set_pi8 (1, 2, 3, 4, 5, 6, 7, 8);
  s2.as_m64 = _mm_set_pi8 (8, 7, 6, 5, 4, 3, 2, 1);
  u.as_m64 = test (s1.as_m64, s2.as_m64);

  for (i = 0; i < 8; i++)
    tmp [i] = __builtin_abs (s1.as_char[i] - s2.as_char[i]);

  for (i = 0; i < 8; i++)
    e.as_short[0] += tmp[i];

  if (u.as_m64 != e.as_m64)
    abort ();
}
