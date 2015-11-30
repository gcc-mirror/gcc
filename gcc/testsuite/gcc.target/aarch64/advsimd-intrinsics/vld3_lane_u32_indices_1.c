#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

uint32x2x3_t
f_vld3_lane_u32 (uint32_t * p, uint32x2x3_t v)
{
  uint32x2x3_t res;
  /* { dg-error "lane 2 out of range 0 - 1" "" { target *-*-* } 0 } */
  res = vld3_lane_u32 (p, v, 2);
  /* { dg-error "lane -1 out of range 0 - 1" "" { target *-*-* } 0 } */
  res = vld3_lane_u32 (p, v, -1);
  return res;
}
