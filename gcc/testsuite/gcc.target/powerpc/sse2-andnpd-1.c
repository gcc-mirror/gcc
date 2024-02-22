/* { dg-do run { target le } } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_andnpd_1
#endif

#include <emmintrin.h>

static __m128d
__attribute__((noinline, unused))
test (__m128d s1, __m128d s2)
{
  return _mm_andnot_pd (s1, s2); 
}

static void
TEST (void)
{
  union128d u, s1, s2;
  long long source1[2]={34545, 95567};
  long long source2[2]={674, 57897};
  long long e[2];
  double d[2];
   
  s1.x = _mm_loadu_pd ((double *)source1);
  s2.x = _mm_loadu_pd ((double *)source2);
  u.x = test (s1.x, s2.x); 
   
  e[0] = (~source1[0]) & source2[0];
  e[1] = (~source1[1]) & source2[1];
  __builtin_memcpy (d, e, sizeof (d));

  if (check_union128d (u, d))
    abort ();
}
