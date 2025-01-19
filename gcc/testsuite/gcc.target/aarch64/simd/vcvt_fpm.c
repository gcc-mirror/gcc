/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+fp8" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
** test_vcvt1_bf16:
**	msr	fpmr, x0
**	bf1cvtl	v0.8h, v0.8b
**	ret
*/
bfloat16x8_t
test_vcvt1_bf16 (mfloat8x8_t a, fpm_t b)
{
  return vcvt1_bf16_mf8_fpm(a, b);
}

/*
** test_high_vcvt1_bf16:
**	msr	fpmr, x0
**	bf1cvtl2	v0.8h, v0.16b
**	ret
*/
bfloat16x8_t
test_high_vcvt1_bf16 (mfloat8x16_t a, fpm_t b)
{
  return vcvt1_high_bf16_mf8_fpm(a, b);
}

/*
** test_low_vcvt1_bf16:
**	msr	fpmr, x0
**	bf1cvtl	v0.8h, v0.8b
**	ret
*/
bfloat16x8_t
test_low_vcvt1_bf16 (mfloat8x16_t a, fpm_t b)
{
  return vcvt1_low_bf16_mf8_fpm(a, b);
}

/*
** test_vcvt1_f16:
**	msr	fpmr, x0
**	f1cvtl	v0.8h, v0.8b
**	ret
*/
float16x8_t
test_vcvt1_f16 (mfloat8x8_t a, fpm_t b)
{
  return vcvt1_f16_mf8_fpm(a, b);
}

/*
** test_high_vcvt1_f16:
**	msr	fpmr, x0
**	f1cvtl2	v0.8h, v0.16b
**	ret
*/
float16x8_t
test_high_vcvt1_f16 (mfloat8x16_t a, fpm_t b)
{
  return vcvt1_high_f16_mf8_fpm(a, b);
}

/*
** test_low_vcvt1_f16:
**	msr	fpmr, x0
**	f1cvtl	v0.8h, v0.8b
**	ret
*/
float16x8_t
test_low_vcvt1_f16 (mfloat8x16_t a, fpm_t b)
{
  return vcvt1_low_f16_mf8_fpm(a, b);
}

/*
** test_vcvt2_bf16:
**	msr	fpmr, x0
**	bf2cvtl	v0.8h, v0.8b
**	ret
*/
bfloat16x8_t
test_vcvt2_bf16 (mfloat8x8_t a, fpm_t b)
{
  return vcvt2_bf16_mf8_fpm(a, b);
}

/*
** test_high_vcvt2_bf16:
**	msr	fpmr, x0
**	bf2cvtl2	v0.8h, v0.16b
**	ret
*/
bfloat16x8_t
test_high_vcvt2_bf16 (mfloat8x16_t a, fpm_t b)
{
  return vcvt2_high_bf16_mf8_fpm(a, b);
}

/*
** test_low_vcvt2_bf16:
**	msr	fpmr, x0
**	bf1cvtl	v0.8h, v0.8b
**	ret
*/
bfloat16x8_t
test_low_vcvt2_bf16 (mfloat8x16_t a, fpm_t b)
{
  return vcvt1_low_bf16_mf8_fpm(a, b);
}

/*
** test_vcvt2_f16:
**	msr	fpmr, x0
**	f2cvtl	v0.8h, v0.8b
**	ret
*/
float16x8_t
test_vcvt2_f16 (mfloat8x8_t a, fpm_t b)
{
  return vcvt2_f16_mf8_fpm(a, b);
}

/*
** test_high_vcvt2_f16:
**	msr	fpmr, x0
**	f2cvtl2	v0.8h, v0.16b
**	ret
*/
float16x8_t
test_high_vcvt2_f16 (mfloat8x16_t a, fpm_t b)
{
  return vcvt2_high_f16_mf8_fpm(a, b);
}

/*
** test_low_vcvt2_f16:
**	msr	fpmr, x0
**	f1cvtl	v0.8h, v0.8b
**	ret
*/
float16x8_t
test_low_vcvt2_f16 (mfloat8x16_t a, fpm_t b)
{
  return vcvt1_low_f16_mf8_fpm(a, b);
}

/*
** test_vcvt_f16:
**	msr	fpmr, x0
**	fcvtn	v0.8b, v0.4h, v1.4h
**	ret
*/
mfloat8x8_t
test_vcvt_f16 (float16x4_t a, float16x4_t b, fpm_t c)
{
  return vcvt_mf8_f16_fpm(a, b, c);
}

/*
** test_vcvtq_f16:
**	msr	fpmr, x0
**	fcvtn	v0.16b, v0.8h, v1.8h
**	ret
*/
mfloat8x16_t
test_vcvtq_f16 (float16x8_t a, float16x8_t b, fpm_t c)
{
  return vcvtq_mf8_f16_fpm(a, b, c);
}

/*
** test_vcvt_f32:
**	msr	fpmr, x0
**	fcvtn	v0.8b, v0.4s, v1.4s
**	ret
*/
mfloat8x8_t
test_vcvt_f32 (float32x4_t a, float32x4_t b, fpm_t c)
{
  return vcvt_mf8_f32_fpm(a, b, c);
}

/*
** test_vcvt_high_f32:
**	msr	fpmr, x0
**	fcvtn2	v0.16b, v1.4s, v2.4s
**	ret
*/
mfloat8x16_t
test_vcvt_high_f32 (mfloat8x8_t a, float32x4_t b, float32x4_t c, fpm_t d)
{
  return vcvt_high_mf8_f32_fpm(a, b, c, d);
}
