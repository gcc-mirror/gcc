/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_andps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 s1, __m128 s2)
{
  return _mm_and_ps (s1, s2); 
}

static void
TEST (void)
{
  union128 u, s1, s2;
  union
  {
    float f[4];
    int   i[4];
  }source1, source2, e;

  s1.x = _mm_set_ps (34, 545, 955, 67);
  s2.x = _mm_set_ps (67, 4, 57, 897);

  _mm_storeu_ps (source1.f, s1.x);
  _mm_storeu_ps (source2.f, s2.x);

  u.x = test (s1.x, s2.x); 
   
  e.i[0] = source1.i[0] & source2.i[0];
  e.i[1] = source1.i[1] & source2.i[1];
  e.i[2] = source1.i[2] & source2.i[2];
  e.i[3] = source1.i[3] & source2.i[3];

  if (check_union128 (u, e.f))
    abort ();
}
