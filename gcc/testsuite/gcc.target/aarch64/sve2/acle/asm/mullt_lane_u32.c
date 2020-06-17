/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mullt_lane_0_u32_tied1:
**	umullt	z0\.s, z0\.h, z1\.h\[0\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_0_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z0, z1, 0),
		    z0_res = svmullt_lane (z0, z1, 0))

/*
** mullt_lane_0_u32_tied2:
**	umullt	z0\.s, z1\.h, z0\.h\[0\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_0_u32_tied2, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z1, z0, 0),
		    z0_res = svmullt_lane (z1, z0, 0))

/*
** mullt_lane_0_u32_untied:
**	umullt	z0\.s, z1\.h, z2\.h\[0\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_0_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z1, z2, 0),
		    z0_res = svmullt_lane (z1, z2, 0))

/*
** mullt_lane_1_u32:
**	umullt	z0\.s, z1\.h, z2\.h\[1\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_1_u32, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z1, z2, 1),
		    z0_res = svmullt_lane (z1, z2, 1))

/*
** mullt_lane_2_u32:
**	umullt	z0\.s, z1\.h, z2\.h\[2\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_2_u32, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z1, z2, 2),
		    z0_res = svmullt_lane (z1, z2, 2))

/*
** mullt_lane_3_u32:
**	umullt	z0\.s, z1\.h, z2\.h\[3\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_3_u32, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z1, z2, 3),
		    z0_res = svmullt_lane (z1, z2, 3))

/*
** mullt_lane_4_u32:
**	umullt	z0\.s, z1\.h, z2\.h\[4\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_4_u32, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z1, z2, 4),
		    z0_res = svmullt_lane (z1, z2, 4))

/*
** mullt_lane_5_u32:
**	umullt	z0\.s, z1\.h, z2\.h\[5\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_5_u32, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z1, z2, 5),
		    z0_res = svmullt_lane (z1, z2, 5))

/*
** mullt_lane_6_u32:
**	umullt	z0\.s, z1\.h, z2\.h\[6\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_6_u32, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z1, z2, 6),
		    z0_res = svmullt_lane (z1, z2, 6))

/*
** mullt_lane_7_u32:
**	umullt	z0\.s, z1\.h, z2\.h\[7\]
**	ret
*/
TEST_TYPE_CHANGE_Z (mullt_lane_7_u32, svuint32_t, svuint16_t,
		    z0_res = svmullt_lane_u32 (z1, z2, 7),
		    z0_res = svmullt_lane (z1, z2, 7))

/*
** mullt_lane_z8_u32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	umullt	z0\.s, z1\.h, \1\.h\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mullt_lane_z8_u32, svuint32_t, svuint16_t, z8,
		    z0 = svmullt_lane_u32 (z1, z8, 1),
		    z0 = svmullt_lane (z1, z8, 1))

/*
** mullt_lane_z16_u32:
**	mov	(z[0-7])\.d, z16\.d
**	umullt	z0\.s, z1\.h, \1\.h\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (mullt_lane_z16_u32, svuint32_t, svuint16_t, z16,
		    z0 = svmullt_lane_u32 (z1, z16, 1),
		    z0 = svmullt_lane (z1, z16, 1))
