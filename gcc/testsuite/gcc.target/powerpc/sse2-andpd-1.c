/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_andpd_1
#endif

#include <emmintrin.h>

static __m128d
__attribute__((noinline, unused))
test (__m128d s1, __m128d s2)
{
  return _mm_and_pd (s1, s2); 
}

static void
TEST (void)
{
  union128d u, s1, s2;
  
  union
  {
    double d[2];
    long long ll[2];
  }source1, source2, e;
  double d[2];
   
  s1.x = _mm_set_pd (34545, 95567);
  s2.x = _mm_set_pd (674, 57897);

  _mm_storeu_pd (source1.d, s1.x);
  _mm_storeu_pd (source2.d, s2.x);

  u.x = test (s1.x, s2.x); 
   
  e.ll[0] = source1.ll[0] & source2.ll[0];
  e.ll[1] = source1.ll[1] & source2.ll[1];
  __builtin_memcpy (d, e.d, sizeof (d));

  if (check_union128d (u, d))
    abort ();
}
