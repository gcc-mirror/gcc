/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" "" {-O[^0]} } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

#include <arm_neon.h>

/*
**test_bfcvtnq2_untied:
**     mov	v0.16b, v1.16b
**     bfcvtn2	v0.8h, v2.4s
**     ret
*/
bfloat16x8_t test_bfcvtnq2_untied (bfloat16x8_t unused, bfloat16x8_t inactive,
                                  float32x4_t a)
{
  return vcvtq_high_bf16_f32 (inactive, a);
}
