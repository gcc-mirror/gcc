/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv9.2-a+sve2")

void
test (svbool_t p, svfloat16_t a, svfloat16_t b)
{
  svamax_f16_m (p, a, b); /* { dg-error {ACLE function 'svamax_f16_m' requires ISA extension 'faminmax'} } */
}
