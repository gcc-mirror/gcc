/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvtps2pd_1
#endif

#include <emmintrin.h>

static __m128d
__attribute__((noinline, unused))
test (__m128 p)
{
  return _mm_cvtps_pd (p); 
}

static void
TEST (void)
{
  union128d u;
  union128 s;
  double e[2];

  s.x = _mm_set_ps (2.78, 7777768.82, 2.331, 3.456);

  u.x = test (s.x);

  e[0] = (double)s.a[0]; 
  e[1] = (double)s.a[1]; 

  if (check_union128d (u, e))
    {
#if DEBUG
      printf ("sse2_test_cvtps2pd_1; check_union128d failed\n");
      printf ("\t cvt\t [%f,%f,%f,%f] -> [%f,%f]\n", s.a[0], s.a[1], s.a[2],
	      s.a[3], u.a[0], u.a[1]);
      printf ("\t expect\t [%f,%f]\n", e[0], e[1]);
#endif
      abort ();
    }
}
