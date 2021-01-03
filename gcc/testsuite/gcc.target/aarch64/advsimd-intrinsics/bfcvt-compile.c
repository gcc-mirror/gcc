/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" "" {-O[^0]} } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

#include <arm_neon.h>

/*
**test_bfcvtn:
**     bfcvtn	v0.4h, v0.4s
**     ret
*/
bfloat16x4_t test_bfcvtn (float32x4_t a)
{
  return vcvt_bf16_f32 (a);
}

/*
**test_bfcvtnq:
**     bfcvtn	v0.4h, v0.4s
**     ret
*/
bfloat16x8_t test_bfcvtnq (float32x4_t a)
{
  return vcvtq_low_bf16_f32 (a);
}

/*
**test_bfcvtnq2:
**     bfcvtn2	v0.8h, v1.4s
**     ret
*/
bfloat16x8_t test_bfcvtnq2 (bfloat16x8_t inactive, float32x4_t a)
{
  return vcvtq_high_bf16_f32 (inactive, a);
}

/*
**test_bfcvt:
**     bfcvt	h0, s0
**     ret
*/
bfloat16_t test_bfcvt (float32_t a)
{
  return vcvth_bf16_f32 (a);
}

/*
**test_vcvt_f32_bf16:
**     shll	v0.4s, v0.4h, #16
**     ret
*/
float32x4_t test_vcvt_f32_bf16 (bfloat16x4_t a)
{
  return vcvt_f32_bf16 (a);
}

/*
**test_vcvtq_low_f32_bf16:
**     shll	v0.4s, v0.4h, #16
**     ret
*/
float32x4_t test_vcvtq_low_f32_bf16 (bfloat16x8_t a)
{
  return vcvtq_low_f32_bf16 (a);
}

/*
**test_vcvtq_high_f32_bf16:
**     shll2	v0.4s, v0.8h, #16
**     ret
*/
float32x4_t test_vcvtq_high_f32_bf16 (bfloat16x8_t a)
{
  return vcvtq_high_f32_bf16 (a);
}

/*
**test_vcvtah_f32_bf16:
**     shl	d0, d0, #16
**     ret
*/
float32_t test_vcvtah_f32_bf16 (bfloat16_t a)
{
  return vcvtah_f32_bf16 (a);
}
