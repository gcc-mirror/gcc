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
#define TEST sse_test_sqrtps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 s1)
{
  return _mm_sqrt_ps (s1); 
}

static void
TEST (void)
{
  union128 u, s1;
  float e[4];
  int i;
   
  s1.x = _mm_set_ps (24.43, 68.346, 43.35, 546.46);
  u.x = test (s1.x); 
  
  for (i = 0; i < 4; i++) {
    __m128 tmp = _mm_load_ss (&s1.a[i]);
    tmp = _mm_sqrt_ss (tmp);
    _mm_store_ss (&e[i], tmp);
    }

  if (check_union128 (u, e))
    abort ();
}
