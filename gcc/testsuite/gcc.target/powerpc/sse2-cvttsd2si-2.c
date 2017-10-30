/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvttsd2si_2
#endif

#include <emmintrin.h>

static long long
__attribute__((noinline, unused))
test (__m128d p)
{
  __asm("" : "+v"(p));
  return _mm_cvttsd_si64 (p); 
}

static void
TEST (void)
{
  union128d s;
  long long e;
  long long d;

  s.x = _mm_set_pd (123.321, 42949672339501.4);

  d = test (s.x);
  e = (long long)(s.a[0]);

  if (d != e)
    abort ();
}
