/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_psrlw_1
#endif

#define N 0xb

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_srli_epi16 (s1, N); 
}

static void
TEST (void)
{
  union128i_w u, s;
  short e[8] = {0};
  unsigned short tmp;
  int i;
 
  s.x = _mm_set_epi16 (1, -2, 3, -4, 5, 6, 0x7000, 0x9000);

  u.x = test (s.x);

  if (N < 16)
    for (i = 0; i < 8; i++)
      {
        tmp = s.a[i];
        e[i] = tmp >> N;
      }

  if (check_union128i_w (u, e))
    abort (); 
}
