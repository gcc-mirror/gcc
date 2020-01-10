/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmulh_lane_0_s16_tied1:
**	sqdmulh	z0\.h, z0\.h, z1\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_0_s16_tied1, svint16_t,
		z0 = svqdmulh_lane_s16 (z0, z1, 0),
		z0 = svqdmulh_lane (z0, z1, 0))

/*
** qdmulh_lane_0_s16_tied2:
**	sqdmulh	z0\.h, z1\.h, z0\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_0_s16_tied2, svint16_t,
		z0 = svqdmulh_lane_s16 (z1, z0, 0),
		z0 = svqdmulh_lane (z1, z0, 0))

/*
** qdmulh_lane_0_s16_untied:
**	sqdmulh	z0\.h, z1\.h, z2\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_0_s16_untied, svint16_t,
		z0 = svqdmulh_lane_s16 (z1, z2, 0),
		z0 = svqdmulh_lane (z1, z2, 0))

/*
** qdmulh_lane_1_s16:
**	sqdmulh	z0\.h, z1\.h, z2\.h\[1\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_1_s16, svint16_t,
		z0 = svqdmulh_lane_s16 (z1, z2, 1),
		z0 = svqdmulh_lane (z1, z2, 1))

/*
** qdmulh_lane_2_s16:
**	sqdmulh	z0\.h, z1\.h, z2\.h\[2\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_2_s16, svint16_t,
		z0 = svqdmulh_lane_s16 (z1, z2, 2),
		z0 = svqdmulh_lane (z1, z2, 2))

/*
** qdmulh_lane_3_s16:
**	sqdmulh	z0\.h, z1\.h, z2\.h\[3\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_3_s16, svint16_t,
		z0 = svqdmulh_lane_s16 (z1, z2, 3),
		z0 = svqdmulh_lane (z1, z2, 3))

/*
** qdmulh_lane_4_s16:
**	sqdmulh	z0\.h, z1\.h, z2\.h\[4\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_4_s16, svint16_t,
		z0 = svqdmulh_lane_s16 (z1, z2, 4),
		z0 = svqdmulh_lane (z1, z2, 4))

/*
** qdmulh_lane_5_s16:
**	sqdmulh	z0\.h, z1\.h, z2\.h\[5\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_5_s16, svint16_t,
		z0 = svqdmulh_lane_s16 (z1, z2, 5),
		z0 = svqdmulh_lane (z1, z2, 5))

/*
** qdmulh_lane_6_s16:
**	sqdmulh	z0\.h, z1\.h, z2\.h\[6\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_6_s16, svint16_t,
		z0 = svqdmulh_lane_s16 (z1, z2, 6),
		z0 = svqdmulh_lane (z1, z2, 6))

/*
** qdmulh_lane_7_s16:
**	sqdmulh	z0\.h, z1\.h, z2\.h\[7\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_7_s16, svint16_t,
		z0 = svqdmulh_lane_s16 (z1, z2, 7),
		z0 = svqdmulh_lane (z1, z2, 7))

/*
** qdmulh_lane_z8_s16:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	sqdmulh	z0\.h, z1\.h, \1\.h\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (qdmulh_lane_z8_s16, svint16_t, svint16_t, z8,
		    z0 = svqdmulh_lane_s16 (z1, z8, 1),
		    z0 = svqdmulh_lane (z1, z8, 1))

/*
** qdmulh_lane_z16_s16:
**	mov	(z[0-7])\.d, z16\.d
**	sqdmulh	z0\.h, z1\.h, \1\.h\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (qdmulh_lane_z16_s16, svint16_t, svint16_t, z16,
		    z0 = svqdmulh_lane_s16 (z1, z16, 1),
		    z0 = svqdmulh_lane (z1, z16, 1))
