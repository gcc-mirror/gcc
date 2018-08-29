/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_movsd_3
#endif

#include <emmintrin.h>

static __m128d
__attribute__((noinline, unused))
test (__m128d a, __m128d b)
{
  __asm("" : "+v"(a), "+v"(b));
  return _mm_move_sd (a, b);
}

static void
TEST (void)
{
  union128d u, s1, s2;
  double e[2] = { 256.046, 3345.1234 };

  s1.x = _mm_setr_pd (128.023, 3345.1234);
  s2.x = _mm_setr_pd (256.046, 4533.1234);
  __asm("" : "+v"(s1.x), "+v"(s2.x));
  u.x = test (s1.x, s2.x);

  if (check_union128d (u, e))
#if DEBUG
  {
      printf ("sse2_test_movsd_3; check_union128d failed\n");
      printf ("\t [%f,%f], [%f,%f] -> [%f,%f]\n", s1.a[0], s1.a[1], s2.a[0],
	      s2.a[1], u.a[0], u.a[1]);
      printf ("\t expect [%f,%f]\n", e[0], e[1]);
  }
#else
    abort ();
#endif
}
