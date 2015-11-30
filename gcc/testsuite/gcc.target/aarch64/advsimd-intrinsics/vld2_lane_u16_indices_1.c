#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

uint16x4x2_t
f_vld2_lane_u16 (uint16_t * p, uint16x4x2_t v)
{
  uint16x4x2_t res;
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  res = vld2_lane_u16 (p, v, 4);
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  res = vld2_lane_u16 (p, v, -1);
  return res;
}
