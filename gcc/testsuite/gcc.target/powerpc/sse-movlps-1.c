/* { dg-do run { target le } } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_movlps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 a, __m64 *p)
{
  __asm("" : "+v"(a));
  return _mm_loadl_pi (a, p);
}

static void
TEST (void)
{
  union128 u, s1;
  float d[2] = {24.43, 68.346};
  float e[4] = {1.17, 2.16, 3.15, 4.14};

  s1.x = _mm_set_ps (5.13, 6.12, 7.11, 8.9);
  u.x = _mm_loadu_ps (e);
 
  u.x = test (s1.x, (__m64 *)d);


  e[0] = d[0];
  e[1] = d[1];
  e[2] = s1.a[2];
  e[3] = s1.a[3];

  if (check_union128 (u, e))
    abort ();
}
