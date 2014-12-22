/* { dg-do assemble } */

#include <arm_neon.h>

uint8_t
test_vget_lane_u8_before (uint8x8_t in)
{
  /* { dg-error "lane -1 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vget_lane_u8 (in, -1);
}

uint8_t
test_vget_lane_u8_beyond (uint8x8_t in)
{
  /* { dg-error "lane 8 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vget_lane_u8 (in, 8);
}
