/* { dg-do assemble } */

#include <arm_neon.h>

uint16_t
test_vgetq_lane_u16_before (uint16x8_t in)
{
  /* { dg-error "lane -1 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vgetq_lane_u16 (in, -1);
}

uint16_t
test_vgetq_lane_u16_beyond (uint16x8_t in)
{
  /* { dg-error "lane 8 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vgetq_lane_u16 (in, 8);
}
