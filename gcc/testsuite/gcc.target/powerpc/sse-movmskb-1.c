/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_movmskb_1
#endif

#include <xmmintrin.h>

static int
__attribute__((noinline, unused))
test (__m64 a)
{
  return _mm_movemask_pi8 (a);
}

static void
TEST (void)
{
  __m64_union u;
  int d;
  int e = 0;
  int i;

  u.as_m64 = _mm_set_pi8 (1,2,3,4,-80,-40,-100,-15);;
  d = test (u.as_m64);

  for (i = 0; i < 8; i++)
    if (u.as_signed_char[i] < 0)
      e |= (1 << i);

  if (d != e)
    abort ();

}
