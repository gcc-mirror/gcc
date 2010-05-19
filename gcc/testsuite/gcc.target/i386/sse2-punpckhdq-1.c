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

static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i s2)
{
  return _mm_unpackhi_epi32 (s1, s2); 
}

static void
TEST (void)
{
  union128i_d u, s1, s2;
  int e[4];
  int i;
   
  s1.x = _mm_set_epi32 (10,20,-80,-40);
  s2.x = _mm_set_epi32 (11, -34, -78, -39);
  u.x = test (s1.x, s2.x); 
   
  for (i = 0; i < 2; i++)
    {
      e[2*i] = s1.a[2+i];
      e[2*i+1] = s2.a[2+i];
    }

  if (check_union128i_d (u, e))
    abort ();
}
