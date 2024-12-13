/* { dg-do assemble { target aarch64_asm_fp8fma_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8fma_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+fp8fma"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+ssve-fp8fma"
#endif

/*
** mlalb_f16_mf8_tied1:
** 	msr	fpmr, x0
**	fmlalb	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (mlalb_f16_mf8_tied1, svfloat16_t, svmfloat8_t,
	     z0 = svmlalb_f16_mf8_fpm (z0, z4, z5, fpm0),
	     z0 = svmlalb_fpm (z0, z4, z5, fpm0))

/*
** mlalb_f16_mf8_tied2:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlalb	z0\.h, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (mlalb_f16_mf8_tied2, svfloat16_t, svmfloat8_t,
		 z0_res = svmlalb_f16_mf8_fpm (z4, z0, z1, fpm0),
		 z0_res = svmlalb_fpm (z4, z0, z1, fpm0))

/*
** mlalb_f16_mf8_tied3:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlalb	z0\.h, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (mlalb_f16_mf8_tied3, svfloat16_t, svmfloat8_t,
		 z0_res = svmlalb_f16_mf8_fpm (z4, z1, z0, fpm0),
		 z0_res = svmlalb_fpm (z4, z1, z0, fpm0))

/*
** mlalb_f16_mf8_untied:
** 	msr	fpmr, x0
**	movprfx	z0, z1
**	fmlalb	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (mlalb_f16_mf8_untied, svfloat16_t, svmfloat8_t,
	     z0 = svmlalb_f16_mf8_fpm (z1, z4, z5, fpm0),
	     z0 = svmlalb_fpm (z1, z4, z5, fpm0))

/*
** mlalb_h7_f16_tied1:
** 	msr	fpmr, x0
**	mov	(z[0-9]+\.b), b7
**	fmlalb	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_ZD (mlalb_h7_f16_tied1, svfloat16_t, svmfloat8_t, mfloat8_t,
	      z0 = svmlalb_n_f16_mf8_fpm (z0, z4, d7, fpm0),
	      z0 = svmlalb_fpm (z0, z4, d7, fpm0))

/*
** mlalb_h7_f16_untied:
** 	msr	fpmr, x0
**	mov	(z[0-9]+\.b), b7
**	movprfx	z0, z1
**	fmlalb	z0\.h, z4\.b, \1
**	ret
*/
TEST_DUAL_ZD (mlalb_h7_f16_untied, svfloat16_t, svmfloat8_t, mfloat8_t,
	      z0 = svmlalb_n_f16_mf8_fpm (z1, z4, d7, fpm0),
	      z0 = svmlalb_fpm (z1, z4, d7, fpm0))
