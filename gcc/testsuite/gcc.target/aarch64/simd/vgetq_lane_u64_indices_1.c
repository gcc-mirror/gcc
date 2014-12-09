/* { dg-do assemble } */

#include <arm_neon.h>

uint64_t
test_vgetq_lane_u64_before (uint64x2_t in)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vgetq_lane_u64 (in, -1);
}

uint64_t
test_vgetq_lane_u64_beyond (uint64x2_t in)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vgetq_lane_u64 (in, 2);
}
