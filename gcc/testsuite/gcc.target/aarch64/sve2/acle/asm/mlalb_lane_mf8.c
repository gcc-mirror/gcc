/* { dg-do assemble { target aarch64_asm_fp8fma_ok } } */
/* { dg-do compile { target { ! aarch64_asm_fp8fma_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+fp8fma"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+ssve-fp8fma"
#endif

/*
** mlalb_lane_0_f16_tied1:
** 	msr	fpmr, x0
**	fmlalb	z0\.h, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (mlalb_lane_0_f16_tied1, svfloat16_t, svmfloat8_t,
	     z0 = svmlalb_lane_f16_mf8_fpm (z0, z4, z5, 0, fpm0),
	     z0 = svmlalb_lane_fpm (z0, z4, z5, 0, fpm0))

/*
** mlalb_lane_0_f16_tied2:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlalb	z0\.h, \1\.b, z1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlalb_lane_0_f16_tied2, svfloat16_t, svmfloat8_t,
		 z0_res = svmlalb_lane_f16_mf8_fpm (z4, z0, z1, 0, fpm0),
		 z0_res = svmlalb_lane_fpm (z4, z0, z1, 0, fpm0))

/*
** mlalb_lane_0_f16_tied3:
** 	msr	fpmr, x0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlalb	z0\.h, z1\.b, \1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlalb_lane_0_f16_tied3, svfloat16_t, svmfloat8_t,
		 z0_res = svmlalb_lane_f16_mf8_fpm (z4, z1, z0, 0, fpm0),
		 z0_res = svmlalb_lane_fpm (z4, z1, z0, 0, fpm0))

/*
** mlalb_lane_0_f16_untied:
** 	msr	fpmr, x0
**	movprfx	z0, z1
**	fmlalb	z0\.h, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (mlalb_lane_0_f16_untied, svfloat16_t, svmfloat8_t,
	     z0 = svmlalb_lane_f16_mf8_fpm (z1, z4, z5, 0, fpm0),
	     z0 = svmlalb_lane_fpm (z1, z4, z5, 0, fpm0))

/*
** mlalb_lane_1_f16:
** 	msr	fpmr, x0
**	fmlalb	z0\.h, z4\.b, z5\.b\[1\]
**	ret
*/
TEST_DUAL_Z (mlalb_lane_1_f16, svfloat16_t, svmfloat8_t,
	     z0 = svmlalb_lane_f16_mf8_fpm (z0, z4, z5, 1, fpm0),
	     z0 = svmlalb_lane_fpm (z0, z4, z5, 1, fpm0))

/*
** mlalb_lane_z8_f16:
**	...
** 	msr	fpmr, x0
**	mov	(z[0-7])\.d, z8\.d
**	fmlalb	z0\.h, z1\.b, \1\.b\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mlalb_lane_z8_f16, svfloat16_t, svmfloat8_t, z8,
		    z0 = svmlalb_lane_f16_mf8_fpm (z0, z1, z8, 1, fpm0),
		    z0 = svmlalb_lane_fpm (z0, z1, z8, 1, fpm0))

/*
** mlalb_lane_z16_f16:
**	...
** 	msr	fpmr, x0
**	mov	(z[0-7])\.d, z16\.d
**	fmlalb	z0\.h, z1\.b, \1\.b\[15\]
**	...
**	ret
*/
TEST_DUAL_LANE_REG (mlalb_lane_z16_f16, svfloat16_t, svmfloat8_t, z16,
		    z0 = svmlalb_lane_f16_mf8_fpm (z0, z1, z16, 15, fpm0),
		    z0 = svmlalb_lane_fpm (z0, z1, z16, 15, fpm0))
