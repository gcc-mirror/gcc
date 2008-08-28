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
#include <math.h>

static __m128d
__attribute__((noinline, unused))
test (__m128d s1)
{
  return _mm_sqrt_pd (s1); 
}

static void
TEST (void)
{
  union128d u, s1;
  double e[2];
  int i;
   
  s1.x = _mm_set_pd (2134.3343,1234.635654);
  u.x = test (s1.x); 

  for (i = 0; i < 2; i++)
    {
      __m128d tmp = _mm_load_sd (&s1.a[i]);
      tmp = _mm_sqrt_sd (tmp, tmp);
      _mm_store_sd (&e[i], tmp);
    }

  if (check_union128d (u, e))
    abort ();
}
