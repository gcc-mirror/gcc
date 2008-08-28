/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

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
  return _mm_shufflehi_epi16 (s1, N); 
}

static void
TEST (void)
{
  union128i_q s1;
  union128i_w u;
  short  e[8] = {0};
  int i;
  int m1[4] = {0x3, 0x3<<2, 0x3<<4, 0x3<<6};
  int m2[4];
  
  s1.x = _mm_set_epi64x (0xabcde,0xef58a234);
  u.x = test (s1.x);

  for (i = 0; i < 4; i++)
    e[i] = (s1.a[0]>>(16 * i)) & 0xffff;

  for (i = 0; i < 4; i++)
    m2[i] = (N & m1[i])>>(2*i);

  for (i = 0; i < 4; i++)
    e[i+4] = (s1.a[1] >> (16 * m2[i])) & 0xffff;

  if (check_union128i_w(u, e))
    abort ();
}
