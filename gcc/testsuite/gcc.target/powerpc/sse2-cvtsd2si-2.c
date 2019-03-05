/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvtsd2si_2
#endif

#include <emmintrin.h>

static long long
__attribute__((noinline, unused))
test (__m128d p)
{
  return _mm_cvtsd_si64 (p); 
}

static void
TEST (void)
{
  union128d s;
  long long e;
  long long d;

  s.x = _mm_set_pd (829496729501.4, 429496729501.4);

  d = test (s.x);

  e = (long long)(s.a[0] + 0.5);

  if (d != e)
    {
#if DEBUG
      printf ("sse2_test_cvtsd2si_2; failed\n");
      printf ("\t [%f,%f] -> [%ld]\n", s.a[0], s.a[1], d);
      printf ("\t expect [%ld]\n", e);
#endif
      abort ();
    }
}
