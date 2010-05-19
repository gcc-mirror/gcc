/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#define N 0xec

#include CHECK_H

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_shuffle_epi32 (s1, N); 
}

static void
TEST (void)
{
  union128i_d u, s1;
  int e[4] = {0};
  int i;
   
  s1.x = _mm_set_epi32 (16,15,14,13);
  u.x = test (s1.x);

  for (i = 0; i < 4; i++)
    e[i] = s1.a[((N & (0x3<<(2*i)))>>(2*i))];

  if (check_union128i_d(u, e))
    abort ();
}
