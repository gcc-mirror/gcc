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
test (__m128 p)
{
  return _mm_cvtps_epi32 (p); 
}

static void
TEST (void)
{
  union128i_d u;
  union128 s;
  int e[4] = {0};

  s.x = _mm_set_ps (2.78, 7777768.82, 2.331, 3.456);

  u.x = test (s.x);

  e[0] = (int)(s.a[0] + 0.5); 
  e[1] = (int)(s.a[1] + 0.5); 
  e[2] = (int)(s.a[2] + 0.5); 
  e[3] = (int)(s.a[3] + 0.5); 

  if (check_union128i_d (u, e))
    abort ();
}
