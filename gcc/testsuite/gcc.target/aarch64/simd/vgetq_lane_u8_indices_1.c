/* { dg-do assemble } */

#include <arm_neon.h>

uint8_t
test_vgetq_lane_u8_before (uint8x16_t in)
{
  /* { dg-error "lane -1 out of range 0 - 15" "" {target *-*-*} 0 } */
  return vgetq_lane_u8 (in, -1);
}

uint8_t
test_vgetq_lane_u8_beyond (uint8x16_t in)
{
  /* { dg-error "lane 16 out of range 0 - 15" "" {target *-*-*} 0 } */
  return vgetq_lane_u8 (in, 16);
}
