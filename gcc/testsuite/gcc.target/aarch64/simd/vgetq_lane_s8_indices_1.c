/* { dg-do assemble } */

#include <arm_neon.h>

int8_t
test_vgetq_lane_s8_before (int8x16_t in)
{
  /* { dg-error "lane -1 out of range 0 - 15" "" {target *-*-*} 0 } */
  return vgetq_lane_s8 (in, -1);
}

int8_t
test_vgetq_lane_s8_beyond (int8x16_t in)
{
  /* { dg-error "lane 16 out of range 0 - 15" "" {target *-*-*} 0 } */
  return vgetq_lane_s8 (in, 16);
}
