/* { dg-do assemble { target aarch64_asm_fp8fma_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8fma_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+fp8fma"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+ssve-fp8fma"
#endif

/*
** mlallbt_f32_mf8_tied1:
** 	msr	fpmr, x0
**	fmlallbt	z0\.s, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (mlallbt_f32_mf8_tied1, svfloat32_t, svmfloat8_t,
	     z0 = svmlallbt_f32_mf8_fpm (z0, z4, z5, fpm0),
	     z0 = svmlallbt_fpm (z0, z4, z5, fpm0))

/*
** mlallbt_f32_mf8_tied2:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlallbt	z0\.s, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (mlallbt_f32_mf8_tied2, svfloat32_t, svmfloat8_t,
		 z0_res = svmlallbt_f32_mf8_fpm (z4, z0, z1, fpm0),
		 z0_res = svmlallbt_fpm (z4, z0, z1, fpm0))

/*
** mlallbt_f32_mf8_tied3:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlallbt	z0\.s, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (mlallbt_f32_mf8_tied3, svfloat32_t, svmfloat8_t,
		 z0_res = svmlallbt_f32_mf8_fpm (z4, z1, z0, fpm0),
		 z0_res = svmlallbt_fpm (z4, z1, z0, fpm0))

/*
** mlallbt_f32_mf8_untied:
** 	msr	fpmr, x0
**	movprfx	z0, z1
**	fmlallbt	z0\.s, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (mlallbt_f32_mf8_untied, svfloat32_t, svmfloat8_t,
	     z0 = svmlallbt_f32_mf8_fpm (z1, z4, z5, fpm0),
	     z0 = svmlallbt_fpm (z1, z4, z5, fpm0))

/*
** mlalb_h7_f16_tied1:
** 	msr	fpmr, x0
**	mov	(z[0-9]+\.b), b7
**	fmlallbt	z0\.s, z4\.b, \1
**	ret
*/
TEST_DUAL_ZD (mlalb_h7_f16_tied1, svfloat32_t, svmfloat8_t, mfloat8_t,
	      z0 = svmlallbt_n_f32_mf8_fpm (z0, z4, d7, fpm0),
	      z0 = svmlallbt_fpm (z0, z4, d7, fpm0))

/*
** mlalb_h7_f16_untied:
** 	msr	fpmr, x0
**	mov	(z[0-9]+\.b), b7
**	movprfx	z0, z1
**	fmlallbt	z0\.s, z4\.b, \1
**	ret
*/
TEST_DUAL_ZD (mlalb_h7_f16_untied, svfloat32_t, svmfloat8_t, mfloat8_t,
	      z0 = svmlallbt_n_f32_mf8_fpm (z1, z4, d7, fpm0),
	      z0 = svmlallbt_fpm (z1, z4, d7, fpm0))
