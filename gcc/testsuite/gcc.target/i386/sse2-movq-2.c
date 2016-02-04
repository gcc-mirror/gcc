/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (long long b)
{
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
