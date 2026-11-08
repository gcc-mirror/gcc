/* { dg-do compile } */
/* { dg-options "-mlsx" } */

#include <lsxintrin.h>

__m128i
test_vbitrevi (__m128i a)
{
  __m128i r;

  r = __lsx_vbitrevi_b (a, 8);	/* { dg-error "invalid argument to built-in function" } */
  r = __lsx_vbitrevi_h (a, 16);	/* { dg-error "invalid argument to built-in function" } */
  r = __lsx_vbitrevi_w (a, 32);	/* { dg-error "invalid argument to built-in function" } */
  r = __lsx_vbitrevi_d (a, 64);	/* { dg-error "invalid argument to built-in function" } */
  return r;
}
