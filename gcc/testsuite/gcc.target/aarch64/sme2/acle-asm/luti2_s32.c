/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** luti2_z1_z0_0:
**	luti2	z1\.s, zt0, z0\[0\]
**	ret
*/
TEST_XN_SINGLE (luti2_z1_z0_0, svint32_t, svuint8_t, z1,
		svluti2_lane_zt_s32 (0, z0, 0),
		svluti2_lane_zt_s32 (0, z0, 0))

/*
** luti2_z18_z5_15:
**	luti2	z18\.s, zt0, z5\[15\]
**	ret
*/
TEST_XN_SINGLE (luti2_z18_z5_15, svint32_t, svuint8_t, z18,
		svluti2_lane_zt_s32 (0, z5, 15),
		svluti2_lane_zt_s32 (0, z5, 15))

/*
** luti2_z24_z7_13:
**	luti2	z24\.s, zt0, z7\[13\]
**	ret
*/
TEST_XN_SINGLE (luti2_z24_z7_13, svint32_t, svuint8_t, z24,
		svluti2_lane_zt_s32 (0, z7, 13),
		svluti2_lane_zt_s32 (0, z7, 13))

/*
** luti2_z28_z16_11:
**	luti2	z28\.s, zt0, z16\[11\]
**	ret
*/
TEST_XN_SINGLE (luti2_z28_z16_11, svint32_t, svuint8_t, z28,
		svluti2_lane_zt_s32 (0, z16, 11),
		svluti2_lane_zt_s32 (0, z16, 11))

/*
** luti2_z24_z23_1:
**	luti2	z24\.s, zt0, z23\[1\]
**	ret
*/
TEST_XN_SINGLE (luti2_z24_z23_1, svint32_t, svuint8_t, z24,
		svluti2_lane_zt_s32 (0, z23, 1),
		svluti2_lane_zt_s32 (0, z23, 1))
