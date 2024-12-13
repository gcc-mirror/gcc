/* { dg-do compile } */
/* { dg-additional-options "-march=armv9-a" } */

#include "arm_neon.h"

void
test (uint8x8_t a, uint8x8_t b)
{
  vluti2_lane_u8 (a, b, 0); /* { dg-error {ACLE function 'vluti2_lane_u8' requires ISA extension 'lut'} } */
}
