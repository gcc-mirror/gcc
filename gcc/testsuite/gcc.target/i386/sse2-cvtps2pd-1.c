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
test (__m128 p)
{
  return _mm_cvtps_pd (p); 
}

static void
TEST (void)
{
  union128d u;
  union128 s;
  double e[2];

  s.x = _mm_set_ps (2.78, 7777768.82, 2.331, 3.456);

  u.x = test (s.x);

  e[0] = (double)s.a[0]; 
  e[1] = (double)s.a[1]; 

  if (check_union128d (u, e))
    abort ();
}
