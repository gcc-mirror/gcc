/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
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

static long long
__attribute__((noinline, unused))
test (__m128 p)
{
  return _mm_cvttss_si64 (p); 
}

static void
TEST (void)
{
  union128 s1;
  long long d;
  long long e;
   
  s1.x = _mm_set_ps (24.43, 68.346, 43.35, 429496729501.4);
  d = test (s1.x); 
  e = (long long)s1.a[0];  

  if (e != d)
    abort ();
}
