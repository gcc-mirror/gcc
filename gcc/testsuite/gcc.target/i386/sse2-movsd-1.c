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

static __m128d
__attribute__((noinline, unused))
test (double *p)
{
  return _mm_load_sd (p); 
}

static void
TEST (void)
{
  union128d u;
  double d[2] = {128.023, 3345.1234};
  double e[2];

  u.x = _mm_loadu_pd (e);
  u.x = test (d);

  e[0] = d[0];
  e[1] = 0.0;

  if (check_union128d (u, e))
    abort ();
}
