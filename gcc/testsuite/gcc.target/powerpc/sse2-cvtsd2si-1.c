/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvtsd2si_1
#endif

#include <emmintrin.h>


static int 
__attribute__((noinline, unused))
test (__m128d p)
{
  return _mm_cvtsd_si32 (p); 
}

static void
TEST (void)
{
  union128d s;
  int e;
  int d;

  s.x = _mm_set_pd (123.321, 456.987);

  d = test (s.x);

  e = (int)(s.a[0] + 0.5);

  if (d != e)
    {
#if DEBUG
      printf ("sse2_test_cvtsd2si_1; failed\n");
      printf ("\t [%f,%f] -> [%d]\n", s.a[0], s.a[1], d);
      printf ("\t expect [%d]\n", e);
#endif
      abort ();
    }
}
