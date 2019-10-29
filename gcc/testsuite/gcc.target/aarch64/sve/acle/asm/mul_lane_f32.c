/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mul_lane_0_f32_tied1:
**	fmul	z0\.s, z0\.s, z1\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_0_f32_tied1, svfloat32_t,
		z0 = svmul_lane_f32 (z0, z1, 0),
		z0 = svmul_lane (z0, z1, 0))

/*
** mul_lane_0_f32_tied2:
**	fmul	z0\.s, z1\.s, z0\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_0_f32_tied2, svfloat32_t,
		z0 = svmul_lane_f32 (z1, z0, 0),
		z0 = svmul_lane (z1, z0, 0))

/*
** mul_lane_0_f32_untied:
**	fmul	z0\.s, z1\.s, z2\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_0_f32_untied, svfloat32_t,
		z0 = svmul_lane_f32 (z1, z2, 0),
		z0 = svmul_lane (z1, z2, 0))

/*
** mul_lane_1_f32:
**	fmul	z0\.s, z1\.s, z2\.s\[1\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_1_f32, svfloat32_t,
		z0 = svmul_lane_f32 (z1, z2, 1),
		z0 = svmul_lane (z1, z2, 1))

/*
** mul_lane_2_f32:
**	fmul	z0\.s, z1\.s, z2\.s\[2\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_2_f32, svfloat32_t,
		z0 = svmul_lane_f32 (z1, z2, 2),
		z0 = svmul_lane (z1, z2, 2))

/*
** mul_lane_3_f32:
**	fmul	z0\.s, z1\.s, z2\.s\[3\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_3_f32, svfloat32_t,
		z0 = svmul_lane_f32 (z1, z2, 3),
		z0 = svmul_lane (z1, z2, 3))

/*
** mul_lane_z7_f32:
**	fmul	z0\.s, z1\.s, z7\.s\[3\]
**	ret
*/
TEST_DUAL_Z (mul_lane_z7_f32, svfloat32_t, svfloat32_t,
	     z0 = svmul_lane_f32 (z1, z7, 3),
	     z0 = svmul_lane (z1, z7, 3))

/*
** mul_lane_z8_f32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	fmul	z0\.s, z1\.s, \1\.s\[3\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mul_lane_z8_f32, svfloat32_t, svfloat32_t, z8,
		    z0 = svmul_lane_f32 (z1, z8, 3),
		    z0 = svmul_lane (z1, z8, 3))
