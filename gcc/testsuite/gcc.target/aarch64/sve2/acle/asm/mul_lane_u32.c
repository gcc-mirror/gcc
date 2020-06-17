/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mul_lane_0_u32_tied1:
**	mul	z0\.s, z0\.s, z1\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_0_u32_tied1, svuint32_t,
		z0 = svmul_lane_u32 (z0, z1, 0),
		z0 = svmul_lane (z0, z1, 0))

/*
** mul_lane_0_u32_tied2:
**	mul	z0\.s, z1\.s, z0\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_0_u32_tied2, svuint32_t,
		z0 = svmul_lane_u32 (z1, z0, 0),
		z0 = svmul_lane (z1, z0, 0))

/*
** mul_lane_0_u32_untied:
**	mul	z0\.s, z1\.s, z2\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_0_u32_untied, svuint32_t,
		z0 = svmul_lane_u32 (z1, z2, 0),
		z0 = svmul_lane (z1, z2, 0))

/*
** mul_lane_1_u32:
**	mul	z0\.s, z1\.s, z2\.s\[1\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_1_u32, svuint32_t,
		z0 = svmul_lane_u32 (z1, z2, 1),
		z0 = svmul_lane (z1, z2, 1))

/*
** mul_lane_2_u32:
**	mul	z0\.s, z1\.s, z2\.s\[2\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_2_u32, svuint32_t,
		z0 = svmul_lane_u32 (z1, z2, 2),
		z0 = svmul_lane (z1, z2, 2))

/*
** mul_lane_3_u32:
**	mul	z0\.s, z1\.s, z2\.s\[3\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_3_u32, svuint32_t,
		z0 = svmul_lane_u32 (z1, z2, 3),
		z0 = svmul_lane (z1, z2, 3))

/*
** mul_lane_z8_u32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	mul	z0\.s, z1\.s, \1\.s\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mul_lane_z8_u32, svuint32_t, svuint32_t, z8,
		    z0 = svmul_lane_u32 (z1, z8, 1),
		    z0 = svmul_lane (z1, z8, 1))

/*
** mul_lane_z16_u32:
**	mov	(z[0-7])\.d, z16\.d
**	mul	z0\.s, z1\.s, \1\.s\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (mul_lane_z16_u32, svuint32_t, svuint32_t, z16,
		    z0 = svmul_lane_u32 (z1, z16, 1),
		    z0 = svmul_lane (z1, z16, 1))
