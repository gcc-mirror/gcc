/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvttsd2si_1
#endif

#include <emmintrin.h>

static int 
__attribute__((noinline, unused))
test (__m128d p)
{
  __asm("" : "+v"(p));
  return _mm_cvttsd_si32 (p); 
}

static void
TEST (void)
{
  union128d s;
  int e;
  int d;

  s.x = _mm_set_pd (123.321, 456.987);

  d = test (s.x);
  e = (int)(s.a[0]);

  if (d != e)
    {
#if DEBUG
      printf ("sse2_test_cvttsd2si_1; failed\n");
      printf ("\t [%f,%f] -> [%d]\n", s.a[0], s.a[1], d);
      printf ("\t expect [%d]\n", e);
#endif
      abort ();
    }
}
