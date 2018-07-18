/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_movaps_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (float *e)
{
  return _mm_load_ps (e); 
}

static void
TEST (void)
{
  union128 u;
  float e[4] __attribute__ ((aligned (16))) = 
    {2134.3343,1234.635654, 1.2234, 876.8976};

  u.x = test (e);   

  if (check_union128 (u, e))
    abort ();
}
