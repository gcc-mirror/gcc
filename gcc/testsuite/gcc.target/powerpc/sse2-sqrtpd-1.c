/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_sqrt_pd_1
#endif

#include <emmintrin.h>
#include <math.h>

static __m128d
__attribute__((noinline, unused))
test (__m128d s1)
{
  return _mm_sqrt_pd (s1); 
}

static void
TEST (void)
{
  union128d u, s1;
  __m128d bogus = { 123.0, 456.0 };
  double e[2];
  int i;
   
  s1.x = _mm_set_pd (2134.3343,1234.635654);
  u.x = test (s1.x); 

  for (i = 0; i < 2; i++)
    {
      __m128d tmp = _mm_load_sd (&s1.a[i]);
      tmp = _mm_sqrt_sd (bogus, tmp);
      _mm_store_sd (&e[i], tmp);
    }

  if (check_union128d (u, e))
#if DEBUG
  {
      printf ("sse2_test_sqrt_pd_1; check_union128d failed\n");
      printf ("\t [%f,%f] -> [%f,%f]\n", s1.a[0], s1.a[1], u.a[0], u.a[1]);
      printf ("\t expect [%f,%f]\n", e[0], e[1]);
  }
#else
    abort ();
#endif
}
