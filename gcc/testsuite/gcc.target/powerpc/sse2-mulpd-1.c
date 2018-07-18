/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_mulpd_1
#endif

#include <emmintrin.h>

static __m128d
__attribute__((noinline, unused))
test (__m128d s1, __m128d s2)
{
  __asm("" : "+v"(s1), "+v"(s2));
  return _mm_mul_pd (s1, s2); 
}

static void
TEST (void)
{
  union128d u, s1, s2;
  double e[2];
   
  s1.x = _mm_set_pd (2134.3343,1234.635654);
  s2.x = _mm_set_pd (41124.234,2344.2354);
  u.x = test (s1.x, s2.x);
   
  e[0] = s1.a[0] * s2.a[0];
  e[1] = s1.a[1] * s2.a[1];

  if (check_union128d (u, e))
#if DEBUG
  {
      printf ("sse2_test_mul_pd_1; check_union128d failed\n");
      printf ("\t [%f,%f] * [%f,%f] -> [%f,%f]\n", s1.a[0], s1.a[1], s2.a[0],
	      s2.a[1], u.a[0], u.a[1]);
      printf ("\t expect [%f,%f]\n", e[0], e[1]);
  }
#else
    abort ();
#endif
}
