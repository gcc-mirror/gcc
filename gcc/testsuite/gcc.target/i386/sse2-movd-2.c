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

static int
__attribute__((noinline, unused))
test (__m128i b)
{
  return _mm_cvtsi128_si32 (b); 
}

static void
TEST (void)
{
  union128i_d u;
  int e;

  u.x = _mm_set_epi32 (2134, -128, 655366, 9999);
  e = test (u.x);
  if (e != u.a[0])
    abort ();
}
