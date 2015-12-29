/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target sse } */
/* { dg-options "-O2 -msse" } */

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#ifndef TEST
#define TEST sse_test
#endif

#include CHECK_H

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128 p, long long b)
{
  return _mm_cvtsi64_ss (p, b); 
}

static void
TEST (void)
{
  union128 u, s1;
  long long b = 4294967295133LL;
  float e[4] = { 24.43, 68.346, 43.35, 546.46 };
   
  s1.x = _mm_set_ps (e[3], e[2], e[1], e[0]);
  u.x = test (s1.x, b); 
  e[0] = (float)b;  

  if (check_union128 (u, e))
    abort ();
}
