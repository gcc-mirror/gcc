/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	sunpk	{z0\.h - z1\.h}, z4\.b
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svint16x2_t, svint8_t, z0,
	      svunpk_s16_s8_x2 (z4),
	      svunpk_s16 (z4))

/*
** unpk_z4_z0:
**	sunpk	{z4\.h - z5\.h}, z0\.b
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svint8_t, svint16x2_t, z4,
	      svunpk_s16_s8_x2 (z0),
	      svunpk_s16 (z0))

/*
** unpk_z18_z23:
**	sunpk	{z18\.h - z19\.h}, z23\.b
**	ret
*/
TEST_DUAL_XN (unpk_z18_z23, svint16x2_t, svint8_t, z18,
	      svunpk_s16_s8_x2 (z23),
	      svunpk_s16 (z23))

/*
** unpk_z23_z28:
**	sunpk	[^\n]+, z28\.b
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svint8_t, svint16x2_t, z23,
	      svunpk_s16_s8_x2 (z28),
	      svunpk_s16 (z28))

/*
** unpk_z28_z4:
**	sunpk	{z28\.h - z29\.h}, z4\.b
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svint16x2_t, svint8_t, z28,
	      svunpk_s16_s8_x2 (z4),
	      svunpk_s16 (z4))
