/* { dg-do run } */
/* { dg-options "-O3 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_movq_2
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (long long b)
{
  __asm("" : "+r" (b));
  return _mm_cvtsi64_si128 (b); 
}

static void
TEST (void)
{
  union128i_q u;
  long long b = 4294967295133LL;
  long long e[2] = {0};

  u.x = test (b);

  e[0] = b;

  if (check_union128i_q (u, e))
    abort ();
}
