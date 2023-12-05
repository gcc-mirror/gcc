/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** cvt_z0_z4:
**	ucvtf	{z0\.s - z1\.s}, {z4\.s - z5\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z0_z4, svfloat32x2_t, svuint32x2_t, z0,
	      svcvt_f32_u32_x2 (z4),
	      svcvt_f32 (z4))

/*
** cvt_z4_z0:
**	ucvtf	{z4\.s - z5\.s}, {z0\.s - z1\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z4_z0, svuint32x2_t, svfloat32x2_t, z4,
	      svcvt_f32_u32_x2 (z0),
	      svcvt_f32 (z0))

/*
** cvt_z18_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	ucvtf	{z18\.s - z19\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z18_z23, svfloat32x2_t, svuint32x2_t, z18,
	      svcvt_f32_u32_x2 (z23),
	      svcvt_f32 (z23))

/*
** cvt_z23_z28:
**	ucvtf	[^\n]+, {z28\.s - z29\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z28, svuint32x2_t, svfloat32x2_t, z23,
	      svcvt_f32_u32_x2 (z28),
	      svcvt_f32 (z28))
