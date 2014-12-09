/* { dg-do assemble } */

#include <arm_neon.h>

uint64_t
test_vget_lane_u64_before (uint64x1_t in)
{
  /* { dg-error "lane -1 out of range 0 - 0" "" {target *-*-*} 0 } */
  return vget_lane_u64 (in, -1);
}

uint64_t
test_vget_lane_u64_beyond (uint64x1_t in)
{
  /* { dg-error "lane 1 out of range 0 - 0" "" {target *-*-*} 0 } */
  return vget_lane_u64 (in, 1);
}
