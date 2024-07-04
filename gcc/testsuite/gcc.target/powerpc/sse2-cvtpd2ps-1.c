/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvtpd_ps
#endif

#include <emmintrin.h>

static __m128
__attribute__((noinline, unused))
test (__m128d p)
{
  return _mm_cvtpd_ps (p); 
}

static void
TEST (void)
{
  union128 u;
  union128d s;
  float e[4] = { 0.0 };

  s.x = _mm_set_pd (123.321, 456.987);

  u.x = test (s.x);

  e[0] = (float)s.a[0]; 
  e[1] = (float)s.a[1]; 

  if (check_union128 (u, e))
    {
#if DEBUG
      printf ("sse2_test_cvtpd_ps; check_union128 failed\n");
      printf ("\t [%f,%f] -> [%f,%f,%f,%f]\n", s.a[0], s.a[1], u.a[0], u.a[1],
	      u.a[2], u.a[3]);
      printf ("\t expect [%f,%f,%f,%f]\n", e[0], e[1], e[2], e[3]);
#endif
      abort ();
    }
}
