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

static void
__attribute__((noinline, unused))
test (__m128i *p, __m128i s)
{
  return _mm_stream_si128 (p, s); 
}

static void
TEST (void)
{
  union128i_d u;
  int e[4] __attribute__ ((aligned(16)));

  u.x = _mm_set_epi32 (21, 34, 334, 8567);

  test ((__m128i *)e, u.x);
  
  if (check_union128i_d (u, e))
    abort ();
}
