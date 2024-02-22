/* { dg-do run } */
/* { dg-options "-O3 -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_cvtss2si_2
#endif

#include <xmmintrin.h>
#include <smmintrin.h>

static long long
__attribute__((noinline, unused))
test (__m128 p)
{
  return _mm_cvtss_si64 (p); 
}

static void
TEST (void)
{
  union128 s1;
  long long d;
  long long e;
  union {
	float f;
	int i;
  } u;
   
  s1.x = _mm_set_ps (344.4, 68.346, 43.35, 429496729501.4);
  d = test (s1.x); 

  u.i = _mm_extract_ps (s1.x, 0);
  e = u.f;

  if (e != d)
    abort ();
}
