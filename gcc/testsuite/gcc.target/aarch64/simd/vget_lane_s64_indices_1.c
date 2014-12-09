/* { dg-do assemble } */

#include <arm_neon.h>

int64_t
test_vget_lane_s64_before (int64x1_t in)
{
  /* { dg-error "lane -1 out of range 0 - 0" "" {target *-*-*} 0 } */
  return vget_lane_s64 (in, -1);
}

int64_t
test_vget_lane_s64_beyond (int64x1_t in)
{
  /* { dg-error "lane 1 out of range 0 - 0" "" {target *-*-*} 0 } */
  return vget_lane_s64 (in, 1);
}
