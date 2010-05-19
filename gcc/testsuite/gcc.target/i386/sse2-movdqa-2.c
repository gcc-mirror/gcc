/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

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
test (__m128i *p, __m128i a)
{
  return _mm_store_si128 (p, a); 
}

static void
TEST (void)
{
  union128i_d u;
  int e[4] __attribute__ ((aligned(16))) = {0};

  u.x = _mm_set_epi32 (1, 2, 3, 4);
   
  test ((__m128i *)e, u.x); 

  if (check_union128i_d (u, e))
    abort ();
}
