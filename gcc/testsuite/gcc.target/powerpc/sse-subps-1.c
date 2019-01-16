/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_subps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 s1, __m128 s2)
{
  return _mm_sub_ps (s1, s2); 
}

static void
TEST (void)
{
  union128 u, s1, s2;
  float e[4];
   
  s1.x = _mm_set_ps (41124.234,6678.346,8653.65635,856.43576);
  s2.x = _mm_set_ps (2134.3343,6678.346,453.345635,54646.464356);
  u.x = test (s1.x, s2.x); 
   
  e[0] = s1.a[0] - s2.a[0];
  e[1] = s1.a[1] - s2.a[1];
  e[2] = s1.a[2] - s2.a[2];
  e[3] = s1.a[3] - s2.a[3];

  if (check_union128 (u, e))
    abort ();
}
