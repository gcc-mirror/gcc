/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** unpk_z0_z4:
**	uunpk	{z0\.s - z1\.s}, z4\.h
**	ret
*/
TEST_DUAL_XN (unpk_z0_z4, svuint32x2_t, svuint16_t, z0,
	      svunpk_u32_u16_x2 (z4),
	      svunpk_u32 (z4))

/*
** unpk_z4_z0:
**	uunpk	{z4\.s - z5\.s}, z0\.h
**	ret
*/
TEST_DUAL_XN (unpk_z4_z0, svuint16_t, svuint32x2_t, z4,
	      svunpk_u32_u16_x2 (z0),
	      svunpk_u32 (z0))

/*
** unpk_z18_z23:
**	uunpk	{z18\.s - z19\.s}, z23\.h
**	ret
*/
TEST_DUAL_XN (unpk_z18_z23, svuint32x2_t, svuint16_t, z18,
	      svunpk_u32_u16_x2 (z23),
	      svunpk_u32 (z23))

/*
** unpk_z23_z28:
**	uunpk	[^\n]+, z28\.h
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (unpk_z23_z28, svuint16_t, svuint32x2_t, z23,
	      svunpk_u32_u16_x2 (z28),
	      svunpk_u32 (z28))

/*
** unpk_z28_z4:
**	uunpk	{z28\.s - z29\.s}, z4\.h
**	ret
*/
TEST_DUAL_XN (unpk_z28_z4, svuint32x2_t, svuint16_t, z28,
	      svunpk_u32_u16_x2 (z4),
	      svunpk_u32 (z4))
