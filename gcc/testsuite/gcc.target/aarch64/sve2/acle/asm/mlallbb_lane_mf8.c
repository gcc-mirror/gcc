/* { dg-do assemble { target aarch64_asm_fp8fma_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8fma_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+fp8fma"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+ssve-fp8fma"
#endif

/*
** mlallbb_lane_0_f16_tied1:
** 	msr	fpmr, x0
**	fmlallbb	z0\.s, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (mlallbb_lane_0_f16_tied1, svfloat32_t, svmfloat8_t,
	     z0 = svmlallbb_lane_f32_mf8_fpm (z0, z4, z5, 0, fpm0),
	     z0 = svmlallbb_lane_fpm (z0, z4, z5, 0, fpm0))

/*
** mlallbb_lane_0_f32_tied2:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlallbb	z0\.s, \1\.b, z1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlallbb_lane_0_f32_tied2, svfloat32_t, svmfloat8_t,
		 z0_res = svmlallbb_lane_f32_mf8_fpm (z4, z0, z1, 0, fpm0),
		 z0_res = svmlallbb_lane_fpm (z4, z0, z1, 0, fpm0))

/*
** mlallbb_lane_0_f32_tied3:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlallbb	z0\.s, z1\.b, \1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlallbb_lane_0_f32_tied3, svfloat32_t, svmfloat8_t,
		 z0_res = svmlallbb_lane_f32_mf8_fpm (z4, z1, z0, 0, fpm0),
		 z0_res = svmlallbb_lane_fpm (z4, z1, z0, 0, fpm0))

/*
** mlallbb_lane_0_f32_untied:
** 	msr	fpmr, x0
**	movprfx	z0, z1
**	fmlallbb	z0\.s, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (mlallbb_lane_0_f32_untied, svfloat32_t, svmfloat8_t,
	     z0 = svmlallbb_lane_f32_mf8_fpm (z1, z4, z5, 0, fpm0),
	     z0 = svmlallbb_lane_fpm (z1, z4, z5, 0, fpm0))

/*
** mlallbb_lane_1_f32:
** 	msr	fpmr, x0
**	fmlallbb	z0\.s, z4\.b, z5\.b\[1\]
**	ret
*/
TEST_DUAL_Z (mlallbb_lane_1_f32, svfloat32_t, svmfloat8_t,
	     z0 = svmlallbb_lane_f32_mf8_fpm (z0, z4, z5, 1, fpm0),
	     z0 = svmlallbb_lane_fpm (z0, z4, z5, 1, fpm0))

/*
** mlallbb_lane_z8_f32:
**	...
** 	msr	fpmr, x0
**	mov	(z[0-7])\.d, z8\.d
**	fmlallbb	z0\.s, z1\.b, \1\.b\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mlallbb_lane_z8_f32, svfloat32_t, svmfloat8_t, z8,
		    z0 = svmlallbb_lane_f32_mf8_fpm (z0, z1, z8, 1, fpm0),
		    z0 = svmlallbb_lane_fpm (z0, z1, z8, 1, fpm0))

/*
** mlallbb_lane_z16_f32:
**	...
** 	msr	fpmr, x0
**	mov	(z[0-7])\.d, z16\.d
**	fmlallbb	z0\.s, z1\.b, \1\.b\[15\]
**	...
**	ret
*/
TEST_DUAL_LANE_REG (mlallbb_lane_z16_f32, svfloat32_t, svmfloat8_t, z16,
		    z0 = svmlallbb_lane_f32_mf8_fpm (z0, z1, z16, 15, fpm0),
		    z0 = svmlallbb_lane_fpm (z0, z1, z16, 15, fpm0))
