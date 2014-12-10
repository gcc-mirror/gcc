/* { dg-do assemble } */

#include <arm_neon.h>

uint32_t
test_vgetq_lane_u32_before (uint32x4_t in)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vgetq_lane_u32 (in, -1);
}

uint32_t
test_vgetq_lane_u32_beyond (uint32x4_t in)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vgetq_lane_u32 (in, 4);
}
