/* { dg-do assemble } */

#include <arm_neon.h>

int64_t
test_vgetq_lane_s64_before (int64x2_t in)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vgetq_lane_s64 (in, -1);
}

int64_t
test_vgetq_lane_s64_beyond (int64x2_t in)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vgetq_lane_s64 (in, 2);
}
