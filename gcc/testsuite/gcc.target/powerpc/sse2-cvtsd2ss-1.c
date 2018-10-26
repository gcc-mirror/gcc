/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvtsd2ss_1
#endif

#include <emmintrin.h>

static __m128 
__attribute__((noinline, unused))
test (__m128 p1, __m128d p2)
{
  return _mm_cvtsd_ss (p1, p2); 
}

static void
TEST (void)
{
  union128d s1;
  union128 u, s2;
  double source1[2] = {123.345, 67.3321};
  float  e[4] = {5633.098, 93.21, 3.34, 4555.2};

  s1.x = _mm_loadu_pd (source1);
  s2.x = _mm_loadu_ps (e);

  __asm("" : "+v"(s1.x), "+v"(s2.x));
  u.x = test(s2.x, s1.x);

  e[0] = (float)source1[0];

  if (check_union128(u, e))
    {
#if DEBUG
      printf ("sse2_test_cvtsd2ss_1; check_union128 failed\n");
      printf ("\t [%f,%f,%f,%f],[%f,%f]\n", s2.a[0], s2.a[1], s2.a[2], s2.a[3],
	      s1.a[0], s1.a[1]);
      printf ("\t -> \t[%f,%f,%f,%f]\n", u.a[0], u.a[1], u.a[2], u.a[3]);
      printf ("\texpect\t[%f,%f,%f,%f]\n", e[0], e[1], e[2], e[3]);
#endif
      abort ();
    }
}
