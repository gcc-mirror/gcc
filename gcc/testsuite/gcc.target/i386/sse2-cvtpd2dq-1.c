/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128d p)
{
  return _mm_cvtpd_epi32 (p); 
}

static void
TEST (void)
{
  union128i_d u;
  union128d s;
  int e[4] = {0};

  s.x = _mm_set_pd (2.78, 7777768.82);

  u.x = test (s.x);

  e[0] = (int)(s.a[0] + 0.5); 
  e[1] = (int)(s.a[1] + 0.5); 

  if (check_union128i_d (u, e))
    abort ();
}
