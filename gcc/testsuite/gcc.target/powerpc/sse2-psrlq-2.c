/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_psrlq_2
#endif

#include <emmintrin.h>

#ifdef _ARCH_PWR8
static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i c)
{
  return _mm_srl_epi64 (s1, c); 
}
#endif

static void
TEST (void)
{
#ifdef _ARCH_PWR8
  union128i_q u, s, c;
  long long e[2] = {0};
  unsigned long long tmp;
  int i;
 
  s.x = _mm_set_epi64x (-1, 0xf);
  c.x = _mm_set_epi64x (60,50);

  __asm("" : "+v"(s.x), "+v"(c.x));
  u.x = test (s.x, c.x);

  if (c.a[0] < 64)
    for (i = 0; i < 2; i++){
      tmp = s.a[i];
      e[i] =tmp >> c.a[0];
    } 

  if (check_union128i_q (u, e))
    abort (); 
#endif
}
