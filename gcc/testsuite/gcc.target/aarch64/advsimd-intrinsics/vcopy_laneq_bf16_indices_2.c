/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */

#include <arm_neon.h>

bfloat16x4_t
test_vcopy_laneq_bf16 (bfloat16x4_t a, bfloat16x8_t b)
{
  bfloat16x4_t res;
  /* { dg-error "lane -1 out of range 0 - 7" "" { target *-*-* } 0 } */
  res = vcopy_laneq_bf16 (a, 1, b, -1);
  /* { dg-error "lane 8 out of range 0 - 7" "" { target *-*-* } 0 } */
  res = vcopy_laneq_bf16 (a, 1, b, 8);
  return res;
}
