/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_cvtpi32x2ps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m64  __A, __m64  __B)
{
  return _mm_cvtpi32x2_ps (__A, __B);
}

static void
TEST (void)
{
  __m64_union s1, s2;
  union128 u, e;
  e.x = _mm_set_ps (546.0, 43.0, -20000.0, 1000.0);

  /* input signed in {1000, -20000, 43, 546}.  */
  s1.as_m64 = _mm_setr_pi32 (1000, -20000);
  s2.as_m64 = _mm_setr_pi32 (43, 546);
   
  u.x = test (s1.as_m64, s2.as_m64);


  if (check_union128 (u, e.a))
    abort ();
}
