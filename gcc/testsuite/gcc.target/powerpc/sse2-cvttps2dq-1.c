/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_cvttps2dq_1
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128 p)
{
  return _mm_cvttps_epi32 (p); 
}

static void
TEST (void)
{
  union128 s;
  union128i_d u;
  int e[4] = {0};

  s.x = _mm_set_ps (123.321, 456.987, 33.56, 7765.321);

  u.x = test (s.x);

  e[0] = (int)s.a[0]; 
  e[1] = (int)s.a[1]; 
  e[2] = (int)s.a[2]; 
  e[3] = (int)s.a[3]; 

  if (check_union128i_d (u, e))
    abort ();
}
