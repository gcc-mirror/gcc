/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_cvtpi16ps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m64  __A)
{
  return _mm_cvtpi16_ps (__A);
}

static void
TEST (void)
{
  __m64_union s1;
  union128 u;
  float e[4] = {1000.0, -20000.0, 45.0, -546.0};

  s1.as_m64 = _mm_setr_pi16 (1000, -20000, 45, -546);
   
  u.x = test (s1.as_m64);

  if (check_union128 (u, e))
    abort ();
}
