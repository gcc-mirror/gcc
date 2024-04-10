/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_movmskpd_1
#endif

#include <emmintrin.h>

#ifdef _ARCH_PWR8
static int
__attribute__((noinline, unused))
test (__m128d p)
{
  __asm("" : "+v"(p));
  return _mm_movemask_pd (p); 
}
#endif

static void
TEST (void)
{
#ifdef _ARCH_PWR8
  double source[2] = {1.234, -2234.23};
  union128d s1;
  int d;
  int e;

  s1.x = _mm_loadu_pd (source);

  d = test (s1.x);

  e = 0;
  if (source[0] < 0)
    e |= 1;
  
  if (source[1] < 0)
    e |= 1 << 1;

  if (checkVi (&d, &e, 1))
    {
#if DEBUG
      printf ("sse2_test_movmskpd_1; check_union128d failed\n");
      printf ("\t [%f,%f] -> [%d]\n", s1.a[0], s1.a[1], d);
      printf ("\t expect [%d]\n", e);
#endif
      abort ();
    }
#endif
}
