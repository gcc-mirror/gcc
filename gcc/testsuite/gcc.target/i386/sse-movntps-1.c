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

static void 
__attribute__((noinline, unused))
test (float *p, __m128 s)
{
  return _mm_stream_ps (p, s); 
}

static void
TEST (void)
{
  union128 u;
  float e[4] __attribute__ ((aligned(16)));

  u.x = _mm_set_ps (24.43, 68.346, 43.35, 546.46);
  test (e, u.x); 
  
  if (check_union128 (u, e))
    abort ();
}
