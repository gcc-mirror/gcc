/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#define N 0xf

#include CHECK_H

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_srai_epi32 (s1, N); 
}

static void
TEST (void)
{
  union128i_d u, s;
  int e[4] = {0};
  int i;
 
  s.x = _mm_set_epi32 (1, -2, 3, 4);

  u.x = test (s.x);

  if (N < 32)
    for (i = 0; i < 4; i++)
      e[i] = s.a[i] >> N; 

  if (check_union128i_d (u, e))
    abort (); 
}
