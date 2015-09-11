#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-excess-errors "" { xfail arm*-*-* } } */
/* { dg-skip-if "" { arm*-*-* } } */

uint64x1x2_t
f_vld2_lane_u64 (uint64_t * p, uint64x1x2_t v)
{
  uint64x1x2_t res;
  /* { dg-error "lane 1 out of range 0 - 0" "" { xfail arm*-*-* } 0 } */
  res = vld2_lane_u64 (p, v, 1);
  /* { dg-error "lane -1 out of range 0 - 0" "" { xfail arm*-*-* } 0 } */
  res = vld2_lane_u64 (p, v, -1);
  return res;
}
