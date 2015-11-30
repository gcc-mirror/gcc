#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

int32x4x3_t
f_vld3q_lane_s32 (int32_t * p, int32x4x3_t v)
{
  int32x4x3_t res;
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  res = vld3q_lane_s32 (p, v, 4);
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  res = vld3q_lane_s32 (p, v, -1);
  return res;
}
