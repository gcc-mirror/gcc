#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

int64x1x4_t
f_vld4_lane_s64 (int64_t * p, int64x1x4_t v)
{
  int64x1x4_t res;
  /* { dg-error "lane 1 out of range 0 - 0" "" { target *-*-* } 0 } */
  res = vld4_lane_s64 (p, v, 1);
  /* { dg-error "lane -1 out of range 0 - 0" "" { target *-*-* } 0 } */
  res = vld4_lane_s64 (p, v, -1);
  return res;
}
