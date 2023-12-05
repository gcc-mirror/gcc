/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** luti4_z1_z0_0:
**	luti4	{[^\n]+}, zt0, z0\[0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (luti4_z1_z0_0, svint16x4_t, svuint8_t, z1,
		svluti4_lane_zt_s16_x4 (0, z0, 0),
		svluti4_lane_zt_s16_x4 (0, z0, 0))

/*
** luti4_z18_z5_1:
**	luti4	{[^\n]+}, zt0, z5\[1\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (luti4_z18_z5_1, svint16x4_t, svuint8_t, z18,
		svluti4_lane_zt_s16_x4 (0, z5, 1),
		svluti4_lane_zt_s16_x4 (0, z5, 1))

/*
** luti4_z24_z7_0:
**	luti4	{z24\.h - z27\.h}, zt0, z7\[0\]
**	ret
*/
TEST_XN_SINGLE (luti4_z24_z7_0, svint16x4_t, svuint8_t, z24,
		svluti4_lane_zt_s16_x4 (0, z7, 0),
		svluti4_lane_zt_s16_x4 (0, z7, 0))

/*
** luti4_z28_z16_1:
**	luti4	{z28\.h - z31\.h}, zt0, z16\[1\]
**	ret
*/
TEST_XN_SINGLE (luti4_z28_z16_1, svint16x4_t, svuint8_t, z28,
		svluti4_lane_zt_s16_x4 (0, z16, 1),
		svluti4_lane_zt_s16_x4 (0, z16, 1))

/*
** luti4_z24_z23_0:
**	luti4	{z24\.h - z27\.h}, zt0, z23\[0\]
**	ret
*/
TEST_XN_SINGLE (luti4_z24_z23_0, svint16x4_t, svuint8_t, z24,
		svluti4_lane_zt_s16_x4 (0, z23, 0),
		svluti4_lane_zt_s16_x4 (0, z23, 0))
