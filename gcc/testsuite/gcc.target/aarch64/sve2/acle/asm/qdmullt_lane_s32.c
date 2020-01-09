/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmullt_lane_0_s32_tied1:
**	sqdmullt	z0\.s, z0\.h, z1\.h\[0\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_0_s32_tied1, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z0, z1, 0),
		    z0_res = svqdmullt_lane (z0, z1, 0))

/*
** qdmullt_lane_0_s32_tied2:
**	sqdmullt	z0\.s, z1\.h, z0\.h\[0\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_0_s32_tied2, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z1, z0, 0),
		    z0_res = svqdmullt_lane (z1, z0, 0))

/*
** qdmullt_lane_0_s32_untied:
**	sqdmullt	z0\.s, z1\.h, z2\.h\[0\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_0_s32_untied, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z1, z2, 0),
		    z0_res = svqdmullt_lane (z1, z2, 0))

/*
** qdmullt_lane_1_s32:
**	sqdmullt	z0\.s, z1\.h, z2\.h\[1\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_1_s32, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z1, z2, 1),
		    z0_res = svqdmullt_lane (z1, z2, 1))

/*
** qdmullt_lane_2_s32:
**	sqdmullt	z0\.s, z1\.h, z2\.h\[2\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_2_s32, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z1, z2, 2),
		    z0_res = svqdmullt_lane (z1, z2, 2))

/*
** qdmullt_lane_3_s32:
**	sqdmullt	z0\.s, z1\.h, z2\.h\[3\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_3_s32, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z1, z2, 3),
		    z0_res = svqdmullt_lane (z1, z2, 3))

/*
** qdmullt_lane_4_s32:
**	sqdmullt	z0\.s, z1\.h, z2\.h\[4\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_4_s32, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z1, z2, 4),
		    z0_res = svqdmullt_lane (z1, z2, 4))

/*
** qdmullt_lane_5_s32:
**	sqdmullt	z0\.s, z1\.h, z2\.h\[5\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_5_s32, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z1, z2, 5),
		    z0_res = svqdmullt_lane (z1, z2, 5))

/*
** qdmullt_lane_6_s32:
**	sqdmullt	z0\.s, z1\.h, z2\.h\[6\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_6_s32, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z1, z2, 6),
		    z0_res = svqdmullt_lane (z1, z2, 6))

/*
** qdmullt_lane_7_s32:
**	sqdmullt	z0\.s, z1\.h, z2\.h\[7\]
**	ret
*/
TEST_TYPE_CHANGE_Z (qdmullt_lane_7_s32, svint32_t, svint16_t,
		    z0_res = svqdmullt_lane_s32 (z1, z2, 7),
		    z0_res = svqdmullt_lane (z1, z2, 7))

/*
** qdmullt_lane_z8_s32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	sqdmullt	z0\.s, z1\.h, \1\.h\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (qdmullt_lane_z8_s32, svint32_t, svint16_t, z8,
		    z0 = svqdmullt_lane_s32 (z1, z8, 1),
		    z0 = svqdmullt_lane (z1, z8, 1))

/*
** qdmullt_lane_z16_s32:
**	mov	(z[0-7])\.d, z16\.d
**	sqdmullt	z0\.s, z1\.h, \1\.h\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (qdmullt_lane_z16_s32, svint32_t, svint16_t, z16,
		    z0 = svqdmullt_lane_s32 (z1, z16, 1),
		    z0 = svqdmullt_lane (z1, z16, 1))
