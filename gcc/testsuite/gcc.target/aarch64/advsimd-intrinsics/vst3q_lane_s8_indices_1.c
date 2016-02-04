#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

void
f_vst3q_lane_s8 (int8_t * p, int8x16x3_t v)
{
  /* { dg-error "lane 16 out of range 0 - 15" "" { target *-*-* } 0 } */
  vst3q_lane_s8 (p, v, 16);
  /* { dg-error "lane -1 out of range 0 - 15" "" { target *-*-* } 0 } */
  vst3q_lane_s8 (p, v, -1);
  return;
}
