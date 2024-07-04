/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	uunpk	{z0\.h - z1\.h}, z4\.b
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svuint16x2_t, svuint8_t, z0,
	      svunpk_u16_u8_x2 (z4),
	      svunpk_u16 (z4))

/*
** unpk_z4_z0:
**	uunpk	{z4\.h - z5\.h}, z0\.b
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svuint8_t, svuint16x2_t, z4,
	      svunpk_u16_u8_x2 (z0),
	      svunpk_u16 (z0))

/*
** unpk_z18_z23:
**	uunpk	{z18\.h - z19\.h}, z23\.b
**	ret
*/
TEST_DUAL_XN (unpk_z18_z23, svuint16x2_t, svuint8_t, z18,
	      svunpk_u16_u8_x2 (z23),
	      svunpk_u16 (z23))

/*
** unpk_z23_z28:
**	uunpk	[^\n]+, z28\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svuint8_t, svuint16x2_t, z23,
	      svunpk_u16_u8_x2 (z28),
	      svunpk_u16 (z28))

/*
** unpk_z28_z4:
**	uunpk	{z28\.h - z29\.h}, z4\.b
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svuint16x2_t, svuint8_t, z28,
	      svunpk_u16_u8_x2 (z4),
	      svunpk_u16 (z4))
