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

static __m128i
__attribute__((noinline, unused))
test (__m128i b)
{
  return _mm_move_epi64 (b); 
}

static void
TEST (void)
{
  union128i_q u, s1;
  long long e[2] = {0};

  s1.x = _mm_set_epi64x(12876, 3376590);
  u.x = test (s1.x);
  e[0] = s1.a[0];

  if (check_union128i_q (u, e))
    abort ();
}
