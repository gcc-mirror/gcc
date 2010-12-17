/* { dg-do run } */
/* { dg-options "-O2 -msse" } */
/* { dg-require-effective-target sse } */

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#ifndef TEST
#define TEST sse_test
#endif

#include CHECK_H

#include <xmmintrin.h>

static void
__attribute__((noinline, unused))
test (float *e, __m128 a)
{
  return _mm_store_ss (e, a); 
}

static void
TEST (void)
{
  union128 u;
  float d[1];
  float e[1];
 
  u.x = _mm_set_ps (2134.3343,1234.635654, 1.2234, 876.8976);

  test (d, u.x);

  e[0] = u.a[0];

  if (checkVf (d, e, 1))
    abort ();
}
