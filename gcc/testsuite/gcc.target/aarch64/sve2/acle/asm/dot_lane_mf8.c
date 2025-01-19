/* { dg-do assemble { target aarch64_asm_fp8dot2_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8dot2_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+fp8dot2"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+ssve-fp8dot2"
#endif

/*
** dot_lane_0_f16_tied1:
** 	msr	fpmr, x0
**	fdot	z0\.h, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_f16_tied1, svfloat16_t, svmfloat8_t,
	     z0 = svdot_lane_f16_mf8_fpm (z0, z4, z5, 0, fpm0),
	     z0 = svdot_lane_fpm (z0, z4, z5, 0, fpm0))

/*
** dot_lane_0_f16_tied2:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fdot	z0\.h, \1\.b, z1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_f16_tied2, svfloat16_t, svmfloat8_t,
		 z0_res = svdot_lane_f16_mf8_fpm (z4, z0, z1, 0, fpm0),
		 z0_res = svdot_lane_fpm (z4, z0, z1, 0, fpm0))

/*
** dot_lane_0_f16_tied3:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fdot	z0\.h, z1\.b, \1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_f16_tied3, svfloat16_t, svmfloat8_t,
		 z0_res = svdot_lane_f16_mf8_fpm (z4, z1, z0, 0, fpm0),
		 z0_res = svdot_lane_fpm (z4, z1, z0, 0, fpm0))

/*
** dot_lane_0_f16_untied:
** 	msr	fpmr, x0
**	movprfx	z0, z1
**	fdot	z0\.h, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_f16_untied, svfloat16_t, svmfloat8_t,
	     z0 = svdot_lane_f16_mf8_fpm (z1, z4, z5, 0, fpm0),
	     z0 = svdot_lane_fpm (z1, z4, z5, 0, fpm0))

/*
** dot_lane_1_f16:
** 	msr	fpmr, x0
**	fdot	z0\.h, z4\.b, z5\.b\[1\]
**	ret
*/
TEST_DUAL_Z (dot_lane_1_f16, svfloat16_t, svmfloat8_t,
	     z0 = svdot_lane_f16_mf8_fpm (z0, z4, z5, 1, fpm0),
	     z0 = svdot_lane_fpm (z0, z4, z5, 1, fpm0))

/*
** dot_lane_z8_f16:
**	...
** 	msr	fpmr, x0
**	mov	(z[0-7])\.d, z8\.d
**	fdot	z0\.h, z1\.b, \1\.b\[1\]
**	ldr	d8, \[sp\], 32
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z8_f16, svfloat16_t, svmfloat8_t, z8,
		    z0 = svdot_lane_f16_mf8_fpm (z0, z1, z8, 1, fpm0),
		    z0 = svdot_lane_fpm (z0, z1, z8, 1, fpm0))

/*
** dot_lane_z16_f16:
**	...
** 	msr	fpmr, x0
**	mov	(z[0-7])\.d, z16\.d
**	fdot	z0\.h, z1\.b, \1\.b\[7\]
**	...
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z16_f16, svfloat16_t, svmfloat8_t, z16,
		    z0 = svdot_lane_f16_mf8_fpm (z0, z1, z16, 7, fpm0),
		    z0 = svdot_lane_fpm (z0, z1, z16, 7, fpm0))

/*
** dot_lane_0_f32_tied1:
** 	msr	fpmr, x0
**	fdot	z0\.s, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_f32_tied1, svfloat32_t, svmfloat8_t,
	     z0 = svdot_lane_f32_mf8_fpm (z0, z4, z5, 0, fpm0),
	     z0 = svdot_lane_fpm (z0, z4, z5, 0, fpm0))

/*
** dot_lane_0_f32_tied2:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fdot	z0\.s, \1\.b, z1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_f32_tied2, svfloat32_t, svmfloat8_t,
		 z0_res = svdot_lane_f32_mf8_fpm (z4, z0, z1, 0, fpm0),
		 z0_res = svdot_lane_fpm (z4, z0, z1, 0, fpm0))

/*
** dot_lane_0_f32_tied3:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fdot	z0\.s, z1\.b, \1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_f32_tied3, svfloat32_t, svmfloat8_t,
		 z0_res = svdot_lane_f32_mf8_fpm (z4, z1, z0, 0, fpm0),
		 z0_res = svdot_lane_fpm (z4, z1, z0, 0, fpm0))

/*
** dot_lane_0_f32_untied:
** 	msr	fpmr, x0
**	movprfx	z0, z1
**	fdot	z0\.s, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_f32_untied, svfloat32_t, svmfloat8_t,
	     z0 = svdot_lane_f32_mf8_fpm (z1, z4, z5, 0, fpm0),
	     z0 = svdot_lane_fpm (z1, z4, z5, 0, fpm0))

/*
** dot_lane_1_f32:
** 	msr	fpmr, x0
**	fdot	z0\.s, z4\.b, z5\.b\[1\]
**	ret
*/
TEST_DUAL_Z (dot_lane_1_f32, svfloat32_t, svmfloat8_t,
	     z0 = svdot_lane_f32_mf8_fpm (z0, z4, z5, 1, fpm0),
	     z0 = svdot_lane_fpm (z0, z4, z5, 1, fpm0))

/*
** dot_lane_z8_f32:
**	...
** 	msr	fpmr, x0
**	mov	(z[0-7])\.d, z8\.d
**	fdot	z0\.s, z1\.b, \1\.b\[1\]
**	ldr	d8, \[sp\], 32
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z8_f32, svfloat32_t, svmfloat8_t, z8,
		    z0 = svdot_lane_f32_mf8_fpm (z0, z1, z8, 1, fpm0),
		    z0 = svdot_lane_fpm (z0, z1, z8, 1, fpm0))

/*
** dot_lane_z32_f32:
**	...
** 	msr	fpmr, x0
**	mov	(z[0-7])\.d, z16\.d
**	fdot	z0\.s, z1\.b, \1\.b\[3\]
**	...
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z32_f32, svfloat32_t, svmfloat8_t, z16,
		    z0 = svdot_lane_f32_mf8_fpm (z0, z1, z16, 3, fpm0),
		    z0 = svdot_lane_fpm (z0, z1, z16, 3, fpm0))
