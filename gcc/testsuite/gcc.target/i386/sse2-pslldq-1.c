/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#define N 0x5

#include CHECK_H

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_slli_si128 (s1, N); 
}

static void
TEST (void)
{
  union128i_b u, s;
  char src[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  char e[16] = {0};
  int i;
   
  s.x = _mm_loadu_si128 ((__m128i *)src);

  u.x = test (s.x);

  for (i = 0; i < 16-N; i++)
    e[i+N] = src[i];

  if (check_union128i_b (u, e))
    abort (); 
}
