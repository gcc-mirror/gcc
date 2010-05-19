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


static __m128 
__attribute__((noinline, unused))
test (__m128 p1, __m128d p2)
{
  return _mm_cvtsd_ss (p1, p2); 
}

static void
TEST (void)
{
  union128d s1;
  union128 u, s2;
  double source1[2] = {123.345, 67.3321};
  float  e[4] = {5633.098, 93.21, 3.34, 4555.2};

  s1.x = _mm_loadu_pd (source1);
  s2.x = _mm_loadu_ps (e);

  u.x = test(s2.x, s1.x);

  e[0] = (float)source1[0];

  if (check_union128(u, e))
    abort ();
}
