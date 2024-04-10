/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvtsi2sd_1
#endif

#include <emmintrin.h>

static __m128d 
__attribute__((noinline, unused))
test (__m128d p, int b)
{
  __asm("" : "+v"(p), "+r"(b));
  return _mm_cvtsi32_sd (p, b); 
}

static void
TEST (void)
{
  union128d u, s;
  int b = 128;
  double e[2];

  s.x = _mm_set_pd (123.321, 456.987);

  u.x = test (s.x, b);
  e[0] = (double)b;
  e[1] = s.a[1];

  if (check_union128d (u, e))
    abort ();
}
