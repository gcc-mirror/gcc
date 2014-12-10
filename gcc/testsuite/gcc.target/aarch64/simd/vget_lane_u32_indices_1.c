/* { dg-do assemble } */

#include <arm_neon.h>

uint32_t
test_vget_lane_u32_before (uint32x2_t in)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vget_lane_u32 (in, -1);
}

uint32_t
test_vget_lane_u32_beyond (uint32x2_t in)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vget_lane_u32 (in, 2);
}
