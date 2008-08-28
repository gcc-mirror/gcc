/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#define N 60

#include CHECK_H

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_slli_epi64 (s1, N); 
}

static void
TEST (void)
{
  union128i_q u, s;
  long long e[2] = {0};
  int i;
 
  s.x = _mm_set_epi64x (-1, 0xf);

  u.x = test (s.x);

  if (N < 64)
    for (i = 0; i < 2; i++)
      e[i] = s.a[i] << N; 

  if (check_union128i_q (u, e))
    abort (); 
}
