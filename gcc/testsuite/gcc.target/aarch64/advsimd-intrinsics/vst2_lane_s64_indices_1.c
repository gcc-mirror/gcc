#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

void
f_vst2_lane_s64 (int64_t * p, int64x1x2_t v)
{
  /* { dg-error "lane 1 out of range 0 - 0" "" { target *-*-* } 0 } */
  vst2_lane_s64 (p, v, 1);
  /* { dg-error "lane -1 out of range 0 - 0" "" { target *-*-* } 0 } */
  vst2_lane_s64 (p, v, -1);
  return;
}
