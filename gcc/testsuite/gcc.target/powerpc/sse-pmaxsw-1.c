/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_pmaxsw_1
#endif

#include <xmmintrin.h>

static __m64
__attribute__((noinline, unused))
test (__m64 s1, __m64 s2)
{
  return _mm_max_pi16 (s1, s2);
}

static void
TEST (void)
{
  __m64_union u, e, s1, s2;
  int i;
   
  s1.as_m64 = _mm_set_pi16 (1,2,3,4);
  s2.as_m64 = _mm_set_pi16 (4,3,2,1);
  u.as_m64 = test (s1.as_m64, s2.as_m64);

  for (i=0; i<4; i++)
    e.as_short[i] = s1.as_short[i]>s2.as_short[i]?s1.as_short[i]:s2.as_short[i];

  if (u.as_m64 != e.as_m64)
    abort ();
}
