/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

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
test (__m128i p)
{
  return _mm_cvtepi32_pd (p); 
}

static void
TEST (void)
{
  union128d u;
  union128i_d s;
  double e[2];

  s.x = _mm_set_epi32 (123, 321, 456, 987);

  u.x = test (s.x);

  e[0] = (double)s.a[0]; 
  e[1] = (double)s.a[1]; 

  if (check_union128d (u, e))
    abort ();
}
