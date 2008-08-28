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
  return _mm_or_pd (s1, s2); 
}

static void
TEST (void)
{
  union128d u, s1, s2;

  union
  {
     double d[2];
     long long ll[2];
  }d1, d2, e;

  s1.x = _mm_set_pd (1234, 44386);
  s2.x = _mm_set_pd (5198, 23098);

  _mm_storeu_pd (d1.d, s1.x);
  _mm_storeu_pd (d2.d, s2.x);

  u.x = test (s1.x, s2.x);
  
  e.ll[0] = d1.ll[0] | d2.ll[0];
  e.ll[1] = d1.ll[1] | d2.ll[1];

  if (check_union128d (u, e.d))
    abort ();
}
