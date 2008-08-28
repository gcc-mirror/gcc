/* { dg-do run } */
/* { dg-options "-O2 -msse" } */

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#ifndef TEST
#define TEST sse_test
#endif

#include CHECK_H

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (float *e)
{
  return _mm_load_ss (e); 
}

static void
TEST (void)
{
  union128 u;
  float e[4] = {1.1, 2.2, 3.3, 4.4};
 
  u.x = _mm_set_ps (2134.3343,1234.635654, 1.2234, 876.8976);

  u.x = test (e);

  e[1] = u.a[1];
  e[2] = u.a[2];
  e[3] = u.a[3];   

  if (check_union128 (u, e))
    abort ();
}
