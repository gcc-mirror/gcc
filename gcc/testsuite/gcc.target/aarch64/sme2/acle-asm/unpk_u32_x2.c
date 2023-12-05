/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	uunpk	{z0\.d - z1\.d}, z4\.s
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svuint64x2_t, svuint32_t, z0,
	      svunpk_u64_u32_x2 (z4),
	      svunpk_u64 (z4))

/*
** unpk_z4_z0:
**	uunpk	{z4\.d - z5\.d}, z0\.s
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svuint32_t, svuint64x2_t, z4,
	      svunpk_u64_u32_x2 (z0),
	      svunpk_u64 (z0))

/*
** unpk_z18_z23:
**	uunpk	{z18\.d - z19\.d}, z23\.s
**	ret
*/
TEST_DUAL_XN (unpk_z18_z23, svuint64x2_t, svuint32_t, z18,
	      svunpk_u64_u32_x2 (z23),
	      svunpk_u64 (z23))

/*
** unpk_z23_z28:
**	uunpk	[^\n]+, z28\.s
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svuint32_t, svuint64x2_t, z23,
	      svunpk_u64_u32_x2 (z28),
	      svunpk_u64 (z28))

/*
** unpk_z28_z4:
**	uunpk	{z28\.d - z29\.d}, z4\.s
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svuint64x2_t, svuint32_t, z28,
	      svunpk_u64_u32_x2 (z4),
	      svunpk_u64 (z4))
