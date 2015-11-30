#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

void
f_vst3q_lane_s64 (int64_t * p, int64x2x3_t v)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" { target *-*-* } 0 } */
  vst3q_lane_s64 (p, v, 2);
  /* { dg-error "lane -1 out of range 0 - 1" "" { target *-*-* } 0 } */
  vst3q_lane_s64 (p, v, -1);
  return;
}
