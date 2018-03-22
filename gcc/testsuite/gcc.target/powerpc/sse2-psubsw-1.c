/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_psubsw_1
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i s2)
{
  __asm("" : "+v"(s1), "+v"(s2));
  return _mm_subs_epi16 (s1, s2); 
}

static void
TEST (void)
{
  union128i_w u, s1, s2;
  short e[8];
  int i, tmp;
   
  s1.x = _mm_set_epi16 (10,20,30,90,-80,-40,-100,-15);
  s2.x = _mm_set_epi16 (11, 98, 76, -100, -34, -78, -39, 14);
  u.x = test (s1.x, s2.x); 
   
  for (i = 0; i < 8; i++)
    {
      tmp = s1.a[i] - s2.a[i];
      
      if (tmp > 32767)
        tmp = 32767;
      if (tmp < -32768)
        tmp = -32768;
      
      e[i] = tmp;
    }

  if (check_union128i_w (u, e))
    abort ();
}
