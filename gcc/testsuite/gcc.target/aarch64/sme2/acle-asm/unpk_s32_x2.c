/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	sunpk	{z0\.d - z1\.d}, z4\.s
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svint64x2_t, svint32_t, z0,
	      svunpk_s64_s32_x2 (z4),
	      svunpk_s64 (z4))

/*
** unpk_z4_z0:
**	sunpk	{z4\.d - z5\.d}, z0\.s
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svint32_t, svint64x2_t, z4,
	      svunpk_s64_s32_x2 (z0),
	      svunpk_s64 (z0))

/*
** unpk_z18_z23:
**	sunpk	{z18\.d - z19\.d}, z23\.s
**	ret
*/
TEST_DUAL_XN (unpk_z18_z23, svint64x2_t, svint32_t, z18,
	      svunpk_s64_s32_x2 (z23),
	      svunpk_s64 (z23))

/*
** unpk_z23_z28:
**	sunpk	[^\n]+, z28\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svint32_t, svint64x2_t, z23,
	      svunpk_s64_s32_x2 (z28),
	      svunpk_s64 (z28))

/*
** unpk_z28_z4:
**	sunpk	{z28\.d - z29\.d}, z4\.s
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svint64x2_t, svint32_t, z28,
	      svunpk_s64_s32_x2 (z4),
	      svunpk_s64 (z4))
