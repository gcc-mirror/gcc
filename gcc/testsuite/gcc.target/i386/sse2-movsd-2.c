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
