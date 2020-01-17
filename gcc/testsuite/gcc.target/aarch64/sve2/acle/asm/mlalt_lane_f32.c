/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlalt_lane_0_f32_tied1:
**	fmlalt	z0\.s, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (mlalt_lane_0_f32_tied1, svfloat32_t, svfloat16_t,
	     z0 = svmlalt_lane_f32 (z0, z4, z5, 0),
	     z0 = svmlalt_lane (z0, z4, z5, 0))

/*
** mlalt_lane_0_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlalt	z0\.s, \1\.h, z1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlalt_lane_0_f32_tied2, svfloat32_t, svfloat16_t,
		 z0_res = svmlalt_lane_f32 (z4, z0, z1, 0),
		 z0_res = svmlalt_lane (z4, z0, z1, 0))

/*
** mlalt_lane_0_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlalt	z0\.s, z1\.h, \1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlalt_lane_0_f32_tied3, svfloat32_t, svfloat16_t,
		 z0_res = svmlalt_lane_f32 (z4, z1, z0, 0),
		 z0_res = svmlalt_lane (z4, z1, z0, 0))

/*
** mlalt_lane_0_f32_untied:
**	movprfx	z0, z1
**	fmlalt	z0\.s, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (mlalt_lane_0_f32_untied, svfloat32_t, svfloat16_t,
	     z0 = svmlalt_lane_f32 (z1, z4, z5, 0),
	     z0 = svmlalt_lane (z1, z4, z5, 0))

/*
** mlalt_lane_1_f32:
**	fmlalt	z0\.s, z4\.h, z5\.h\[1\]
**	ret
*/
TEST_DUAL_Z (mlalt_lane_1_f32, svfloat32_t, svfloat16_t,
	     z0 = svmlalt_lane_f32 (z0, z4, z5, 1),
	     z0 = svmlalt_lane (z0, z4, z5, 1))

/*
** mlalt_lane_z8_f32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	fmlalt	z0\.s, z1\.h, \1\.h\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mlalt_lane_z8_f32, svfloat32_t, svfloat16_t, z8,
		    z0 = svmlalt_lane_f32 (z0, z1, z8, 1),
		    z0 = svmlalt_lane (z0, z1, z8, 1))

/*
** mlalt_lane_z16_f32:
**	mov	(z[0-7])\.d, z16\.d
**	fmlalt	z0\.s, z1\.h, \1\.h\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (mlalt_lane_z16_f32, svfloat32_t, svfloat16_t, z16,
		    z0 = svmlalt_lane_f32 (z0, z1, z16, 1),
		    z0 = svmlalt_lane (z0, z1, z16, 1))
