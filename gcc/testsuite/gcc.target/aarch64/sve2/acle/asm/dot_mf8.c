/* { dg-do assemble { target aarch64_asm_fp8dot2_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8dot2_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+fp8dot2"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+ssve-fp8dot2"
#endif

/*
** dot_f16_mf8_tied1:
** 	msr	fpmr, x0
**	fdot	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (dot_f16_mf8_tied1, svfloat16_t, svmfloat8_t,
	     z0 = svdot_f16_mf8_fpm (z0, z4, z5, fpm0),
	     z0 = svdot_fpm (z0, z4, z5, fpm0))

/*
** dot_f16_mf8_tied2:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fdot	z0\.h, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (dot_f16_mf8_tied2, svfloat16_t, svmfloat8_t,
		 z0_res = svdot_f16_mf8_fpm (z4, z0, z1, fpm0),
		 z0_res = svdot_fpm (z4, z0, z1, fpm0))

/*
** dot_f16_mf8_tied3:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fdot	z0\.h, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (dot_f16_mf8_tied3, svfloat16_t, svmfloat8_t,
		 z0_res = svdot_f16_mf8_fpm (z4, z1, z0, fpm0),
		 z0_res = svdot_fpm (z4, z1, z0, fpm0))

/*
** dot_f16_mf8_untied:
** 	msr	fpmr, x0
**	movprfx	z0, z1
**	fdot	z0\.h, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (dot_f16_mf8_untied, svfloat16_t, svmfloat8_t,
	     z0 = svdot_f16_mf8_fpm (z1, z4, z5, fpm0),
	     z0 = svdot_fpm (z1, z4, z5, fpm0))

/*
** dot_f32_mf8_tied1:
** 	msr	fpmr, x0
**	fdot	z0\.s, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (dot_f32_mf8_tied1, svfloat32_t, svmfloat8_t,
	     z0 = svdot_f32_mf8_fpm (z0, z4, z5, fpm0),
	     z0 = svdot_fpm (z0, z4, z5, fpm0))

/*
** dot_f32_mf8_tied2:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fdot	z0\.s, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (dot_f32_mf8_tied2, svfloat32_t, svmfloat8_t,
		 z0_res = svdot_f32_mf8_fpm (z4, z0, z1, fpm0),
		 z0_res = svdot_fpm (z4, z0, z1, fpm0))

/*
** dot_f32_mf8_tied3:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fdot	z0\.s, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (dot_f32_mf8_tied3, svfloat32_t, svmfloat8_t,
		 z0_res = svdot_f32_mf8_fpm (z4, z1, z0, fpm0),
		 z0_res = svdot_fpm (z4, z1, z0, fpm0))

/*
** dot_f32_mf8_untied:
** 	msr	fpmr, x0
**	movprfx	z0, z1
**	fdot	z0\.s, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (dot_f32_mf8_untied, svfloat32_t, svmfloat8_t,
	     z0 = svdot_f32_mf8_fpm (z1, z4, z5, fpm0),
	     z0 = svdot_fpm (z1, z4, z5, fpm0))

