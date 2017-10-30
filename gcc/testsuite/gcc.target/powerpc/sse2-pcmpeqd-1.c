/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_pcmpeqd_1
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i s2)
{
  return _mm_cmpeq_epi32 (s1, s2); 
}

static void
TEST (void)
{
  union128i_d u, s1, s2;
  int e[4];
  int i;
   
  s1.x = _mm_set_epi32 (98, 25, 98,7);
  s2.x = _mm_set_epi32 (88, 44, 33, 229);
  u.x = test (s1.x, s2.x); 
   
  for (i = 0; i < 4; i++)
     e[i] = (s1.a[i] == s2.a[i]) ? -1:0;

  if (check_union128i_d (u, e))
    abort ();
}
