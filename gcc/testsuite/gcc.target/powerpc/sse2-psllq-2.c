/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_psllq_2
#endif

#include <emmintrin.h>

#ifdef _ARCH_PWR8
static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i c)
{
  return _mm_sll_epi64 (s1, c); 
}
#endif

static void
TEST (void)
{
#ifdef _ARCH_PWR8
  union128i_q u, s, c;
  long long e[2] = {0};
  int i;
 
  s.x = _mm_set_epi64x (-1, 0xf);
  c.x = _mm_set_epi64x (60,50);

  __asm("" : "+v"(s.x), "+v"(c.x));
  u.x = test (s.x, c.x);

  if (c.a[0] < 64)
    for (i = 0; i < 2; i++)
      e[i] = s.a[i] << c.a[0]; 

  if (check_union128i_q (u, e))
    abort (); 
#endif
}
