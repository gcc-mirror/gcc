/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvtepi32_ps
#endif

#include <emmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128i p)
{
  return _mm_cvtepi32_ps (p); 
}

static void
TEST (void)
{
  union128 u;
  union128i_d s;
  float e[4];

  s.x = _mm_set_epi32 (123, 321, 456, 987);

  u.x = test (s.x);

  e[0] = (float)s.a[0]; 
  e[1] = (float)s.a[1]; 
  e[2] = (float)s.a[2]; 
  e[3] = (float)s.a[3]; 

  if (check_union128 (u, e))
    abort ();
}
