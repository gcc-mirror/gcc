/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_cvtsi2ss_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 p, int b)
{
  return _mm_cvtsi32_ss (p, b); 
}

static void
TEST (void)
{
  union128 u, s1;
  int b = 498;
  float e[4] = { 24.43, 68.346, 43.35, 546.46 };
   
  s1.x = _mm_set_ps (e[3], e[2], e[1], e[0]);
  u.x = test (s1.x, b); 
  e[0] = (float)b;  

  if (check_union128 (u, e))
    abort ();
}
