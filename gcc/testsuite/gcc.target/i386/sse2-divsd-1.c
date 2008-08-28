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

static __m128d
__attribute__((noinline, unused))
test (__m128d s1, __m128d s2)
{
  return _mm_div_sd (s1, s2); 
}

static void
TEST (void)
{
  union128d u, s1, s2;
  double e[2];
   
  s1.x = _mm_set_pd (2134.3343,1234.635654);
  s2.x = _mm_set_pd (41124.234,2344.2354);
  u.x = test (s1.x, s2.x); 
   
  e[0] = s1.a[0] / s2.a[0];
  e[1] = s1.a[1];

  if (check_union128d (u, e))
    abort ();
}
