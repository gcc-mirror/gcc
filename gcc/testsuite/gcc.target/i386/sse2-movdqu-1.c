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
test (__m128i *p)
{
  return _mm_loadu_si128 (p); 
}

static void
TEST (void)
{
  union128i_d u;
  int e[4]  = {1, 2, 3, 4};
   
  u.x = test ((__m128i *)e); 

  if (check_union128i_d (u, e))
    abort ();
}
