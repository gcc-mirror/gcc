/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_movss_1
#endif

#include <xmmintrin.h>

static __m128
__attribute__((noinline, unused))
test (float *e)
{
  __m128 result;

  __asm("" : "+b"(e));

  result = _mm_load_ss (e);
  __asm("" : "+v"(result));
  return result;
}

static void
TEST (void)
{
  union128 u, s1;
  float e[4] = {1.1, 2.2, 3.3, 4.4};
 
  s1.x = _mm_set_ps (2134.3343,1234.635654, 1.2234, 876.8976);

  u.x = s1.x;
  u.x = test (e);

  e[1] = u.a[1];
  e[2] = u.a[2];
  e[3] = u.a[3];   

  if (check_union128 (u, e))
    abort ();
}
