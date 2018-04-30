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
#define TEST sse_test_cvtpi32ps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128  __A, __m64  __B)
{
  return _mm_cvtpi32_ps (__A, __B);
}

static void
TEST (void)
{
  __m64_union s1;
  union128 s2, u;
  float e[4] = {1000.0, -20000.0, 43.35, 546.46};

  s1.as_m64 = _mm_setr_pi32 (1000, -20000);
   
  s2.x = _mm_setr_ps (24.43, 68.346, 43.35, 546.46);
  u.x = test (s2.x, s1.as_m64);


  if (check_union128 (u, e))
    abort ();
}
