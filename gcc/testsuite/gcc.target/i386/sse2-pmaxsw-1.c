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
  return _mm_max_epi16 (s1, s2); 
}

static void
TEST (void)
{
  union128i_w u, s1, s2;
  short e[8];
  int i;
   
  s1.x = _mm_set_epi16 (1,2,3,4,5,6,7,8);
  s2.x = _mm_set_epi16 (8,7,6,5,4,3,2,1);
  u.x = test (s1.x, s2.x);

  for (i=0; i<8; i++)
    e[i] = s1.a[i]>s2.a[i]?s1.a[i]:s2.a[i]; 

  if (check_union128i_w (u, e))
    abort ();
}
