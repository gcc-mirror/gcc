/* { dg-do compile } */
/* { dg-options "-O0 -mavx512f" } */

#include <x86intrin.h>

int
test_rounding (__m128d x, int r)
{
  return _mm_cvt_roundsd_i32 (x, r); /* { dg-error "incorrect rounding operand." } */
}
