/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	sunpk	{z0\.h - z3\.h}, {z4\.b - z5\.b}
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svint16x4_t, svint8x2_t, z0,
	      svunpk_s16_s8_x4 (z4),
	      svunpk_s16 (z4))

/*
** unpk_z4_z0:
**	sunpk	{z4\.h - z7\.h}, {z0\.b - z1\.b}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svint8x2_t, svint16x4_t, z4,
	      svunpk_s16_s8_x4 (z0),
	      svunpk_s16 (z0))

/*
** unpk_z4_z18:
**	sunpk	{z4\.h - z7\.h}, {z18\.b - z19\.b}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z18, svint8x2_t, svint16x4_t, z4,
	      svunpk_s16_s8_x4 (z18),
	      svunpk_s16 (z18))

/*
** unpk_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	sunpk	{z28\.h - z31\.h}, [^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z28_z23, svint16x4_t, svint8x2_t, z28,
	      svunpk_s16_s8_x4 (z23),
	      svunpk_s16 (z23))

/*
** unpk_z23_z28:
**	sunpk	[^\n]+, {z28\.b - z29\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svint8x2_t, svint16x4_t, z23,
	      svunpk_s16_s8_x4 (z28),
	      svunpk_s16 (z28))

/*
** unpk_z23_z18:
**	sunpk	{z[^\n]+}, {z18\.b - z19\.b}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z18, svint8x2_t, svint16x4_t, z23,
	      svunpk_s16_s8_x4 (z18),
	      svunpk_s16 (z18))

/*
** unpk_z28_z4:
**	sunpk	{z28\.h - z31\.h}, {z4\.b - z5\.b}
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svint16x4_t, svint8x2_t, z28,
	      svunpk_s16_s8_x4 (z4),
	      svunpk_s16 (z4))
