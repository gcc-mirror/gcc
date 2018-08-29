/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_pmaddwd_1
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i s2)
{
  __asm("" : "+v"(s1), "+v"(s2));
  return _mm_madd_epi16 (s1, s2); 
}

static void
TEST (void)
{
  union128i_w  s1, s2;
  union128i_d u;
  int e[4]; 
  int i; 
 
  s1.x = _mm_set_epi16 (2134,3343,1234,6354, 1, 3, 4, 5);
  s2.x = _mm_set_epi16 (41124,234,2344,2354,9, -1, -8, -10);
  u.x = test (s1.x, s2.x); 

  for (i = 0; i < 4; i++)
    e[i] = (s1.a[i*2] * s2.a[i*2])+(s1.a[(i*2) + 1] * s2.a[(i*2) + 1]);   

  if (check_union128i_d (u, e))
    abort ();
}
