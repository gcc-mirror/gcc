#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

uint32x4x4_t
f_vld4q_lane_u32 (uint32_t * p, uint32x4x4_t v)
{
  uint32x4x4_t res;
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  res = vld4q_lane_u32 (p, v, 4);
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  res = vld4q_lane_u32 (p, v, -1);
  return res;
}
