/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_psrlq_1
#endif

#define N 60

#include <emmintrin.h>

#ifdef _ARCH_PWR8
static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_srli_epi64 (s1, N); 
}
#endif

static void
TEST (void)
{
#ifdef _ARCH_PWR8
  union128i_q u, s;
  long long e[2] = {0};
  unsigned long long tmp;
  int i;
 
  s.x = _mm_set_epi64x (-1, 0xf);

  u.x = test (s.x);

  if (N < 64)
    for (i = 0; i < 2; i++) {
      tmp = s.a[i]; 
      e[i] = tmp >> N;
    }

  if (check_union128i_q (u, e))
    abort (); 
#endif
}
