#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

uint64x2x4_t
f_vld4q_lane_u64 (uint64_t * p, uint64x2x4_t v)
{
  uint64x2x4_t res;
  /* { dg-error "lane 2 out of range 0 - 1" "" { target *-*-* } 0 } */
  res = vld4q_lane_u64 (p, v, 2);
  /* { dg-error "lane -1 out of range 0 - 1" "" { target *-*-* } 0 } */
  res = vld4q_lane_u64 (p, v, -1);
  return res;
}
