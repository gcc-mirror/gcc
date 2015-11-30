#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

void
f_vst2_lane_s8 (int8_t * p, int8x8x2_t v)
{
  /* { dg-error "lane 8 out of range 0 - 7" "" { target *-*-* } 0 } */
  vst2_lane_s8 (p, v, 8);
  /* { dg-error "lane -1 out of range 0 - 7" "" { target *-*-* } 0 } */
  vst2_lane_s8 (p, v, -1);
  return;
}
