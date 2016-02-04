#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

int32x2x2_t
f_vld2_lane_s32 (int32_t * p, int32x2x2_t v)
{
  int32x2x2_t res;
  /* { dg-error "lane 2 out of range 0 - 1" "" { target *-*-* } 0 } */
  res = vld2_lane_s32 (p, v, 2);
  /* { dg-error "lane -1 out of range 0 - 1" "" { target *-*-* } 0 } */
  res = vld2_lane_s32 (p, v, -1);
  return res;
}
