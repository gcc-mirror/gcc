/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */

#include <arm_neon.h>

bfloat16x4_t
test_vcopy_lane_bf16 (bfloat16x4_t a, bfloat16x4_t b)
{
  bfloat16x4_t res;
  res = vcopy_lane_bf16 (a, -1, b, 2);
  res = vcopy_lane_bf16 (a, 4, b, 2);
  return res;
}

/* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
/* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
