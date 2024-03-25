/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** luti4_z1_z0_0:
**	luti4	z1\.s, zt0, z0\[0\]
**	ret
*/
TEST_XN_SINGLE (luti4_z1_z0_0, svfloat32_t, svuint8_t, z1,
		svluti4_lane_zt_f32 (0, z0, 0),
		svluti4_lane_zt_f32 (0, z0, 0))

/*
** luti4_z18_z5_7:
**	luti4	z18\.s, zt0, z5\[7\]
**	ret
*/
TEST_XN_SINGLE (luti4_z18_z5_7, svfloat32_t, svuint8_t, z18,
		svluti4_lane_zt_f32 (0, z5, 7),
		svluti4_lane_zt_f32 (0, z5, 7))

/*
** luti4_z24_z7_6:
**	luti4	z24\.s, zt0, z7\[6\]
**	ret
*/
TEST_XN_SINGLE (luti4_z24_z7_6, svfloat32_t, svuint8_t, z24,
		svluti4_lane_zt_f32 (0, z7, 6),
		svluti4_lane_zt_f32 (0, z7, 6))

/*
** luti4_z28_z16_4:
**	luti4	z28\.s, zt0, z16\[4\]
**	ret
*/
TEST_XN_SINGLE (luti4_z28_z16_4, svfloat32_t, svuint8_t, z28,
		svluti4_lane_zt_f32 (0, z16, 4),
		svluti4_lane_zt_f32 (0, z16, 4))

/*
** luti4_z24_z23_1:
**	luti4	z24\.s, zt0, z23\[1\]
**	ret
*/
TEST_XN_SINGLE (luti4_z24_z23_1, svfloat32_t, svuint8_t, z24,
		svluti4_lane_zt_f32 (0, z23, 1),
		svluti4_lane_zt_f32 (0, z23, 1))
