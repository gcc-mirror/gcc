/* { dg-do assemble { target aarch64_asm_f8f32mm_ok } } */
/* { dg-do compile { target { ! aarch64_asm_f8f32mm_ok } } } */
/* { dg-additional-options "-O2 -march=armv8-a+f8f32mm -save-temps -moverride=tune=cheap_fpmr_write" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** fmmla_f32f8mm_tied:
**	msr	fpmr, x0
**	fmmla	v0.4s, v1.16b, v2.16b
**	ret
*/
float32x4_t
fmmla_f32f8mm_tied (float32x4_t v0, mfloat8x16_t v1, mfloat8x16_t v2, fpm_t fpm0)
{
  return vmmlaq_f32_mf8 (v0, v1, v2, fpm0);
}

/*
** fmmla_f32f8mm:
**	msr	fpmr, x0
**	fmmla	v1.4s, v2.16b, v3.16b
**	mov	v0.16b, v1.16b
**	ret
*/
float32x4_t
fmmla_f32f8mm (float32x4_t v0, float32x4_t v1, mfloat8x16_t v2, mfloat8x16_t v3, fpm_t fpm0)
{
  return vmmlaq_f32_mf8 (v1, v2, v3, fpm0);
}
