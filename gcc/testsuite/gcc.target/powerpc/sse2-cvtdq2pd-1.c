/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvtepi32_pd
#endif

#include <emmintrin.h>
#ifdef _ARCH_PWR8
static __m128d
__attribute__((noinline, unused))
test (__m128i p)
{
  return _mm_cvtepi32_pd (p); 
}
#endif

static void
TEST (void)
{
#ifdef _ARCH_PWR8
  union128d u;
  union128i_d s;
  double e[2];

  s.x = _mm_set_epi32 (123, 321, 456, 987);

  u.x = test (s.x);

  e[0] = (double)s.a[0]; 
  e[1] = (double)s.a[1]; 

  if (check_union128d (u, e))
#if DEBUG
  {
      printf ("sse2_test_cvtepi32_pd; check_union128d failed\n");
      printf ("\t [%d,%d, %d, %d] -> [%f,%f]\n",
    		  s.a[0], s.a[1], s.a[2], s.a[3],
			  u.a[0], u.a[1]);
      printf ("\t expect [%f,%f]\n",
			  e[0], e[1]);
  }
#else
    abort ();
#endif
#endif
}
