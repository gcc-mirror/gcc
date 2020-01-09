/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmulh_lane_0_s32_tied1:
**	sqdmulh	z0\.s, z0\.s, z1\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_0_s32_tied1, svint32_t,
		z0 = svqdmulh_lane_s32 (z0, z1, 0),
		z0 = svqdmulh_lane (z0, z1, 0))

/*
** qdmulh_lane_0_s32_tied2:
**	sqdmulh	z0\.s, z1\.s, z0\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_0_s32_tied2, svint32_t,
		z0 = svqdmulh_lane_s32 (z1, z0, 0),
		z0 = svqdmulh_lane (z1, z0, 0))

/*
** qdmulh_lane_0_s32_untied:
**	sqdmulh	z0\.s, z1\.s, z2\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_0_s32_untied, svint32_t,
		z0 = svqdmulh_lane_s32 (z1, z2, 0),
		z0 = svqdmulh_lane (z1, z2, 0))

/*
** qdmulh_lane_1_s32:
**	sqdmulh	z0\.s, z1\.s, z2\.s\[1\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_1_s32, svint32_t,
		z0 = svqdmulh_lane_s32 (z1, z2, 1),
		z0 = svqdmulh_lane (z1, z2, 1))

/*
** qdmulh_lane_2_s32:
**	sqdmulh	z0\.s, z1\.s, z2\.s\[2\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_2_s32, svint32_t,
		z0 = svqdmulh_lane_s32 (z1, z2, 2),
		z0 = svqdmulh_lane (z1, z2, 2))

/*
** qdmulh_lane_3_s32:
**	sqdmulh	z0\.s, z1\.s, z2\.s\[3\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_3_s32, svint32_t,
		z0 = svqdmulh_lane_s32 (z1, z2, 3),
		z0 = svqdmulh_lane (z1, z2, 3))

/*
** qdmulh_lane_z8_s32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	sqdmulh	z0\.s, z1\.s, \1\.s\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (qdmulh_lane_z8_s32, svint32_t, svint32_t, z8,
		    z0 = svqdmulh_lane_s32 (z1, z8, 1),
		    z0 = svqdmulh_lane (z1, z8, 1))

/*
** qdmulh_lane_z16_s32:
**	mov	(z[0-7])\.d, z16\.d
**	sqdmulh	z0\.s, z1\.s, \1\.s\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (qdmulh_lane_z16_s32, svint32_t, svint32_t, z16,
		    z0 = svqdmulh_lane_s32 (z1, z16, 1),
		    z0 = svqdmulh_lane (z1, z16, 1))
