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
#define TEST sse_test_movss_3
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 a, __m128 b)
{
  __asm("" : "+v"(a));
  return _mm_move_ss (a, b); 
}

static void
TEST (void)
{
  union128 u, s1, s2;
  float e[4];
 
  s1.x = _mm_set_ps (2134.3343,1234.635654, 1.2234, 876.8976);
  s2.x = _mm_set_ps (1.1, 2.2, 3.3, 4.4);
  u.x = _mm_set_ps (5.5, 6.6, 7.7, 8.8);
  u.x = test (s1.x, s2.x);
  
  e[0] = s2.a[0];
  e[1] = s1.a[1];
  e[2] = s1.a[2];
  e[3] = s1.a[3];

  if (check_union128 (u, e))
    abort ();
}
