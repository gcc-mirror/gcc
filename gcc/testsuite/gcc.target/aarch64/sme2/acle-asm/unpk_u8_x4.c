/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	uunpk	{z0\.h - z3\.h}, {z4\.b - z5\.b}
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svuint16x4_t, svuint8x2_t, z0,
	      svunpk_u16_u8_x4 (z4),
	      svunpk_u16 (z4))

/*
** unpk_z4_z0:
**	uunpk	{z4\.h - z7\.h}, {z0\.b - z1\.b}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svuint8x2_t, svuint16x4_t, z4,
	      svunpk_u16_u8_x4 (z0),
	      svunpk_u16 (z0))

/*
** unpk_z4_z18:
**	uunpk	{z4\.h - z7\.h}, {z18\.b - z19\.b}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z18, svuint8x2_t, svuint16x4_t, z4,
	      svunpk_u16_u8_x4 (z18),
	      svunpk_u16 (z18))

/*
** unpk_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	uunpk	{z28\.h - z31\.h}, [^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z28_z23, svuint16x4_t, svuint8x2_t, z28,
	      svunpk_u16_u8_x4 (z23),
	      svunpk_u16 (z23))

/*
** unpk_z23_z28:
**	uunpk	[^\n]+, {z28\.b - z29\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svuint8x2_t, svuint16x4_t, z23,
	      svunpk_u16_u8_x4 (z28),
	      svunpk_u16 (z28))

/*
** unpk_z23_z18:
**	uunpk	{z[^\n]+}, {z18\.b - z19\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z18, svuint8x2_t, svuint16x4_t, z23,
	      svunpk_u16_u8_x4 (z18),
	      svunpk_u16 (z18))

/*
** unpk_z28_z4:
**	uunpk	{z28\.h - z31\.h}, {z4\.b - z5\.b}
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svuint16x4_t, svuint8x2_t, z28,
	      svunpk_u16_u8_x4 (z4),
	      svunpk_u16 (z4))
