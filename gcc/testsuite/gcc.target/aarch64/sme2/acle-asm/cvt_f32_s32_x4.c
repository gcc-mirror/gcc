/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** cvt_z0_z4:
**	scvtf	{z0\.s - z3\.s}, {z4\.s - z7\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z0_z4, svfloat32x4_t, svint32x4_t, z0,
	      svcvt_f32_s32_x4 (z4),
	      svcvt_f32 (z4))

/*
** cvt_z4_z0:
**	scvtf	{z4\.s - z7\.s}, {z0\.s - z3\.s}
**	ret
*/
TEST_DUAL_XN (cvt_z4_z0, svint32x4_t, svfloat32x4_t, z4,
	      svcvt_f32_s32_x4 (z0),
	      svcvt_f32 (z0))

/*
** cvt_z4_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	scvtf	{z4\.s - z7\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z4_z18, svint32x4_t, svfloat32x4_t, z4,
	      svcvt_f32_s32_x4 (z18),
	      svcvt_f32 (z18))

/*
** cvt_z28_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	scvtf	{z28\.s - z31\.s}, [^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z28_z23, svfloat32x4_t, svint32x4_t, z28,
	      svcvt_f32_s32_x4 (z23),
	      svcvt_f32 (z23))

/*
** cvt_z23_z28:
**	scvtf	[^\n]+, {z28\.s - z31\.s}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z28, svint32x4_t, svfloat32x4_t, z23,
	      svcvt_f32_s32_x4 (z28),
	      svcvt_f32 (z28))

/*
** cvt_z23_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	scvtf	{z[^\n]+}, {z.*}
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_DUAL_XN (cvt_z23_z18, svint32x4_t, svfloat32x4_t, z23,
	      svcvt_f32_s32_x4 (z18),
	      svcvt_f32 (z18))
