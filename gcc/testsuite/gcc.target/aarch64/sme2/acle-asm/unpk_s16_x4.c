/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	sunpk	{z0\.s - z3\.s}, {z4\.h - z5\.h}
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svint32x4_t, svint16x2_t, z0,
	      svunpk_s32_s16_x4 (z4),
	      svunpk_s32 (z4))

/*
** unpk_z4_z0:
**	sunpk	{z4\.s - z7\.s}, {z0\.h - z1\.h}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svint16x2_t, svint32x4_t, z4,
	      svunpk_s32_s16_x4 (z0),
	      svunpk_s32 (z0))

/*
** unpk_z4_z18:
**	sunpk	{z4\.s - z7\.s}, {z18\.h - z19\.h}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z18, svint16x2_t, svint32x4_t, z4,
	      svunpk_s32_s16_x4 (z18),
	      svunpk_s32 (z18))

/*
** unpk_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	sunpk	{z28\.s - z31\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z28_z23, svint32x4_t, svint16x2_t, z28,
	      svunpk_s32_s16_x4 (z23),
	      svunpk_s32 (z23))

/*
** unpk_z23_z28:
**	sunpk	[^\n]+, {z28\.h - z29\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svint16x2_t, svint32x4_t, z23,
	      svunpk_s32_s16_x4 (z28),
	      svunpk_s32 (z28))

/*
** unpk_z23_z18:
**	sunpk	{z[^\n]+}, {z18\.h - z19\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z18, svint16x2_t, svint32x4_t, z23,
	      svunpk_s32_s16_x4 (z18),
	      svunpk_s32 (z18))

/*
** unpk_z28_z4:
**	sunpk	{z28\.s - z31\.s}, {z4\.h - z5\.h}
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svint32x4_t, svint16x2_t, z28,
	      svunpk_s32_s16_x4 (z4),
	      svunpk_s32 (z4))
