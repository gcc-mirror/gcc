/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	sunpk	{z0\.d - z3\.d}, {z4\.s - z5\.s}
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svint64x4_t, svint32x2_t, z0,
	      svunpk_s64_s32_x4 (z4),
	      svunpk_s64 (z4))

/*
** unpk_z4_z0:
**	sunpk	{z4\.d - z7\.d}, {z0\.s - z1\.s}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svint32x2_t, svint64x4_t, z4,
	      svunpk_s64_s32_x4 (z0),
	      svunpk_s64 (z0))

/*
** unpk_z4_z18:
**	sunpk	{z4\.d - z7\.d}, {z18\.s - z19\.s}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z18, svint32x2_t, svint64x4_t, z4,
	      svunpk_s64_s32_x4 (z18),
	      svunpk_s64 (z18))

/*
** unpk_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	sunpk	{z28\.d - z31\.d}, [^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z28_z23, svint64x4_t, svint32x2_t, z28,
	      svunpk_s64_s32_x4 (z23),
	      svunpk_s64 (z23))

/*
** unpk_z23_z28:
**	sunpk	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svint32x2_t, svint64x4_t, z23,
	      svunpk_s64_s32_x4 (z28),
	      svunpk_s64 (z28))

/*
** unpk_z23_z18:
**	sunpk	{z[^\n]+}, {z18\.s - z19\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z18, svint32x2_t, svint64x4_t, z23,
	      svunpk_s64_s32_x4 (z18),
	      svunpk_s64 (z18))

/*
** unpk_z28_z4:
**	sunpk	{z28\.d - z31\.d}, {z4\.s - z5\.s}
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svint64x4_t, svint32x2_t, z28,
	      svunpk_s64_s32_x4 (z4),
	      svunpk_s64 (z4))
