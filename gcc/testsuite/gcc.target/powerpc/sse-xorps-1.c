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
#define TEST sse_test_xorps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 s1, __m128 s2)
{
  return _mm_xor_ps (s1, s2); 
}

static void
TEST (void)
{
  union {
    float f[4];
    int   i[4];
  }source1, source2, e;

  union128 u, s1, s2;
  int i;
   
  s1.x = _mm_set_ps (24.43, 68.346, 43.35, 546.46);
  s2.x = _mm_set_ps (1.17, 2.16, 3.15, 4.14);

  _mm_storeu_ps (source1.f, s1.x);
  _mm_storeu_ps (source2.f, s2.x);

  u.x = test (s1.x, s2.x); 
  
  for (i = 0; i < 4; i++)
    e.i[i] = source1.i[i] ^ source2.i[i];   

  if (check_union128 (u, e.f))
    abort ();
}
