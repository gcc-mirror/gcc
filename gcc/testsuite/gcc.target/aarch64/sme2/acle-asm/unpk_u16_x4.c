/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	uunpk	{z0\.s - z3\.s}, {z4\.h - z5\.h}
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svuint32x4_t, svuint16x2_t, z0,
	      svunpk_u32_u16_x4 (z4),
	      svunpk_u32 (z4))

/*
** unpk_z4_z0:
**	uunpk	{z4\.s - z7\.s}, {z0\.h - z1\.h}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svuint16x2_t, svuint32x4_t, z4,
	      svunpk_u32_u16_x4 (z0),
	      svunpk_u32 (z0))

/*
** unpk_z4_z18:
**	uunpk	{z4\.s - z7\.s}, {z18\.h - z19\.h}
**	ret
*/
TEST_DUAL_XN (unpk_z4_z18, svuint16x2_t, svuint32x4_t, z4,
	      svunpk_u32_u16_x4 (z18),
	      svunpk_u32 (z18))

/*
** unpk_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	uunpk	{z28\.s - z31\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z28_z23, svuint32x4_t, svuint16x2_t, z28,
	      svunpk_u32_u16_x4 (z23),
	      svunpk_u32 (z23))

/*
** unpk_z23_z28:
**	uunpk	[^\n]+, {z28\.h - z29\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svuint16x2_t, svuint32x4_t, z23,
	      svunpk_u32_u16_x4 (z28),
	      svunpk_u32 (z28))

/*
** unpk_z23_z18:
**	uunpk	{z[^\n]+}, {z18\.h - z19\.h}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z18, svuint16x2_t, svuint32x4_t, z23,
	      svunpk_u32_u16_x4 (z18),
	      svunpk_u32 (z18))

/*
** unpk_z28_z4:
**	uunpk	{z28\.s - z31\.s}, {z4\.h - z5\.h}
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svuint32x4_t, svuint16x2_t, z28,
	      svunpk_u32_u16_x4 (z4),
	      svunpk_u32 (z4))
