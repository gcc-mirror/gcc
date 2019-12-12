/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_movsd_2
#endif

#include <emmintrin.h>

static void 
__attribute__((noinline, unused))
test (double *p, __m128d a)
{
  _mm_store_sd (p, a); 
}

static void
TEST (void)
{
  union128d u;
  double d[1];
  double e[1];

  u.x = _mm_set_pd (128.023, 3345.1234);
  test (d, u.x);

  e[0] = u.a[0];

  if (checkVd (d, e, 1))
    abort ();
}
