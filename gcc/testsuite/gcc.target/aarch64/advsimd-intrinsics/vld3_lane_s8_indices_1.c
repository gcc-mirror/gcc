#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

int8x8x3_t
f_vld3_lane_s8 (int8_t * p, int8x8x3_t v)
{
  int8x8x3_t res;
  /* { dg-error "lane 8 out of range 0 - 7" "" { target *-*-* } 0 } */
  res = vld3_lane_s8 (p, v, 8);
  /* { dg-error "lane -1 out of range 0 - 7" "" { target *-*-* } 0 } */
  res = vld3_lane_s8 (p, v, -1);
  return res;
}
