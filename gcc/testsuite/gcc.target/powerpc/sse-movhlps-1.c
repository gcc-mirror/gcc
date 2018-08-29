/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_movhlps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 s1, __m128 s2)
{
  __asm("" : "+v"(s1), "+v"(s2));
  return _mm_movehl_ps (s1, s2); 
}

static void
TEST (void)
{
  union128 u, s1, s2;
  float e[4];
   
  s1.x = _mm_set_ps (24.43, 68.346, 43.35, 546.46);
  s2.x = _mm_set_ps (1.17, 2.16, 3.15, 4.14);
  u.x = test (s1.x, s2.x); 

  e[0] = s2.a[2];
  e[1] = s2.a[3];
  e[2] = s1.a[2];
  e[3] = s1.a[3];

  if (check_union128 (u, e))
    abort ();
}
