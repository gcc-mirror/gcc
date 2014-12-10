/* { dg-do assemble } */

#include <arm_neon.h>

uint16_t
test_vget_lane_u16_before (uint16x4_t in)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vget_lane_u16 (in, -1);
}

uint16_t
test_vget_lane_u16_beyond (uint16x4_t in)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vget_lane_u16 (in, 4);
}
