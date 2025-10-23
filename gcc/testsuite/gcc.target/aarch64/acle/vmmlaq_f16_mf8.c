/* { dg-do assemble { target aarch64_asm_f8f16mm_ok } } */
/* { dg-do compile { target { ! aarch64_asm_f8f16mm_ok } } } */
/* { dg-additional-options "-O2 -march=armv8-a+f8f16mm -save-temps -moverride=tune=cheap_fpmr_write" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** fmmla_f16f8mm_tied:
**	msr	fpmr, x0
**	fmmla	v0.8h, v1.16b, v2.16b
**	ret
*/
float16x8_t
fmmla_f16f8mm_tied (float16x8_t v0, mfloat8x16_t v1, mfloat8x16_t v2, fpm_t fpm0)
{
  return vmmlaq_f16_mf8 (v0, v1, v2, fpm0);
}

/*
** fmmla_f16f8mm:
**	msr	fpmr, x0
**	fmmla	v1.8h, v2.16b, v3.16b
**	mov	v0.16b, v1.16b
**	ret
*/
float16x8_t
fmmla_f16f8mm (float16x8_t v0, float16x8_t v1, mfloat8x16_t v2, mfloat8x16_t v3, fpm_t fpm0)
{
  return vmmlaq_f16_mf8 (v1, v2, v3, fpm0);
}
