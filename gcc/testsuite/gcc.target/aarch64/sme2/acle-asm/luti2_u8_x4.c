/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** luti2_z1_z0_0:
**	luti2	{[^\n]+}, zt0, z0\[0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (luti2_z1_z0_0, svuint8x4_t, svuint8_t, z1,
		svluti2_lane_zt_u8_x4 (0, z0, 0),
		svluti2_lane_zt_u8_x4 (0, z0, 0))

/*
** luti2_z18_z5_3:
**	luti2	{[^\n]+}, zt0, z5\[3\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (luti2_z18_z5_3, svuint8x4_t, svuint8_t, z18,
		svluti2_lane_zt_u8_x4 (0, z5, 3),
		svluti2_lane_zt_u8_x4 (0, z5, 3))

/*
** luti2_z24_z7_2:
**	luti2	{z24\.b - z27\.b}, zt0, z7\[2\]
**	ret
*/
TEST_XN_SINGLE (luti2_z24_z7_2, svuint8x4_t, svuint8_t, z24,
		svluti2_lane_zt_u8_x4 (0, z7, 2),
		svluti2_lane_zt_u8_x4 (0, z7, 2))

/*
** luti2_z28_z16_1:
**	luti2	{z28\.b - z31\.b}, zt0, z16\[1\]
**	ret
*/
TEST_XN_SINGLE (luti2_z28_z16_1, svuint8x4_t, svuint8_t, z28,
		svluti2_lane_zt_u8_x4 (0, z16, 1),
		svluti2_lane_zt_u8_x4 (0, z16, 1))

/*
** luti2_z24_z23_0:
**	luti2	{z24\.b - z27\.b}, zt0, z23\[0\]
**	ret
*/
TEST_XN_SINGLE (luti2_z24_z23_0, svuint8x4_t, svuint8_t, z24,
		svluti2_lane_zt_u8_x4 (0, z23, 0),
		svluti2_lane_zt_u8_x4 (0, z23, 0))
