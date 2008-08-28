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
test (double *e, __m128d a)
{
  _mm_store_pd (e, a); 
}

static void
TEST (void)
{
  union128d u;
  double e[2] __attribute__ ((aligned (8))) = {0.0};

  u.x = _mm_set_pd (2134.3343,1234.635654);

  test (e, u.x);

  if (check_union128d (u, e))
    abort ();
}
