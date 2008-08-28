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
test (double *e)
{
  return _mm_load_pd (e); 
}

static void
TEST (void)
{
  union128d u;
  double e[2] __attribute__ ((aligned (8))) = {2134.3343,1234.635654};

  u.x = test (e);   

  if (check_union128d (u, e))
    abort ();
}
