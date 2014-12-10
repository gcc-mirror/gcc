/* { dg-do assemble } */

#include <arm_neon.h>

poly16_t
test_vgetq_lane_p16_before (poly16x8_t in)
{
  /* { dg-error "lane -1 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vgetq_lane_p16 (in, -1);
}

poly16_t
test_vgetq_lane_p16_beyond (poly16x8_t in)
{
  /* { dg-error "lane 8 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vgetq_lane_p16 (in, 8);
}
