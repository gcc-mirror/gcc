/* { dg-do assemble } */

#include <arm_neon.h>

poly8_t
test_vgetq_lane_p8_before (poly8x16_t in)
{
  /* { dg-error "lane -1 out of range 0 - 15" "" {target *-*-*} 0 } */
  return vgetq_lane_p8 (in, -1);
}

poly8_t
test_vgetq_lane_p8_beyond (poly8x16_t in)
{
  /* { dg-error "lane 16 out of range 0 - 15" "" {target *-*-*} 0 } */
  return vgetq_lane_p8 (in, 16);
}
