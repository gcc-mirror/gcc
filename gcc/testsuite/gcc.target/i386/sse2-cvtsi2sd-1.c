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
test (__m128d p, int b)
{
  return _mm_cvtsi32_sd (p, b); 
}

static void
TEST (void)
{
  union128d u, s;
  int b = 128;
  double e[2];

  s.x = _mm_set_pd (123.321, 456.987);

  u.x = test (s.x, b);
  e[0] = (double)b;
  e[1] = s.a[1];
  if (check_union128d (u, e))
    abort ();
}
