/* { dg-do run } */
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
test (int b)
{
  return _mm_cvtsi32_si128 (b); 
}

static void
TEST (void)
{
  union128i_d u;
  int b = 128;
  int e[4] = {0};

  u.x = test (b);

  e[0] = b;

  if (check_union128i_d (u, e))
    abort ();
}
