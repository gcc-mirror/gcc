/* { dg-do run { target { ! { ia32 } } } } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <emmintrin.h>

static long long
__attribute__((noinline, unused))
test (__m128d p)
{
  return _mm_cvtsd_si64 (p); 
}

static void
TEST (void)
{
  union128d s;
  long long e;
  long long d;

  s.x = _mm_set_pd (829496729501.4, 429496729501.4);

  d = test (s.x);

  e = (long long)(s.a[0] + 0.5);

  if (d != e)
    abort ();
}
