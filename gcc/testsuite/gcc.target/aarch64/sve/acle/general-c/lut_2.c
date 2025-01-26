/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv9.2-a+sve2")

void
test (svfloat16_t f16, svuint8_t u8)
{
  svluti2_lane (f16, u8, 0); /* { dg-error {ACLE function 'svluti2_lane_f16' requires ISA extension 'lut'} } */
}
