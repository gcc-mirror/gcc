/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_xorpd_1
#endif

#include <emmintrin.h>

static __m128d
__attribute__((noinline, unused))
test (__m128d s1, __m128d s2)
{
  return _mm_xor_pd (s1, s2); 
}

static void
TEST (void)
{
  union
    {
      double d[2];
      long long l[2];
    }source1, source2, e;

  union128d u, s1, s2;
  int i; 
   
  s1.x = _mm_set_pd (11.1321456, 2.287332);
  s2.x = _mm_set_pd (3.37768, 4.43222234);

  _mm_storeu_pd (source1.d, s1.x);
  _mm_storeu_pd (source2.d, s2.x);

  u.x = test (s1.x, s2.x); 
 
  for (i = 0; i < 2; i++)
    e.l[i] = source1.l[i] ^ source2.l[i];

  if (check_union128d (u, e.d))
    abort ();
}
