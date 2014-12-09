/* { dg-do assemble } */

#include <arm_neon.h>

int16_t
test_vgetq_lane_s16_before (int16x8_t in)
{
  /* { dg-error "lane -1 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vgetq_lane_s16 (in, -1);
}

int16_t
test_vgetq_lane_s16_beyond (int16x8_t in)
{
  /* { dg-error "lane 8 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vgetq_lane_s16 (in, 8);
}
