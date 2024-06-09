/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** luti2_z1_z0_0:
**	luti2	{[^\n]+}, zt0, z0\[0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_XN_SINGLE (luti2_z1_z0_0, svuint8x2_t, svuint8_t, z1,
		svluti2_lane_zt_u8_x2 (0, z0, 0),
		svluti2_lane_zt_u8_x2 (0, z0, 0))

/*
** luti2_z18_z5_7:
**	luti2	{z18\.b - z19\.b}, zt0, z5\[7\]
**	ret
*/
TEST_XN_SINGLE (luti2_z18_z5_7, svuint8x2_t, svuint8_t, z18,
		svluti2_lane_zt_u8_x2 (0, z5, 7),
		svluti2_lane_zt_u8_x2 (0, z5, 7))

/*
** luti2_z24_z7_6:
**	luti2	{z24\.b - z25\.b}, zt0, z7\[6\]
**	ret
*/
TEST_XN_SINGLE (luti2_z24_z7_6, svuint8x2_t, svuint8_t, z24,
		svluti2_lane_zt_u8_x2 (0, z7, 6),
		svluti2_lane_zt_u8_x2 (0, z7, 6))

/*
** luti2_z28_z16_3:
**	luti2	{z28\.b - z29\.b}, zt0, z16\[3\]
**	ret
*/
TEST_XN_SINGLE (luti2_z28_z16_3, svuint8x2_t, svuint8_t, z28,
		svluti2_lane_zt_u8_x2 (0, z16, 3),
		svluti2_lane_zt_u8_x2 (0, z16, 3))

/*
** luti2_z24_z23_1:
**	luti2	{z24\.b - z25\.b}, zt0, z23\[1\]
**	ret
*/
TEST_XN_SINGLE (luti2_z24_z23_1, svuint8x2_t, svuint8_t, z24,
		svluti2_lane_zt_u8_x2 (0, z23, 1),
		svluti2_lane_zt_u8_x2 (0, z23, 1))
