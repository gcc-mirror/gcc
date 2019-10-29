/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** set2_f32_z24_0:
**	mov	z25\.d, z5\.d
**	mov	z24\.d, z0\.d
**	ret
*/
TEST_SET (set2_f32_z24_0, svfloat32x2_t, svfloat32_t,
	  z24 = svset2_f32 (z4, 0, z0),
	  z24 = svset2 (z4, 0, z0))

/*
** set2_f32_z24_1:
**	mov	z24\.d, z4\.d
**	mov	z25\.d, z0\.d
**	ret
*/
TEST_SET (set2_f32_z24_1, svfloat32x2_t, svfloat32_t,
	  z24 = svset2_f32 (z4, 1, z0),
	  z24 = svset2 (z4, 1, z0))

/*
** set2_f32_z4_0:
**	mov	z4\.d, z0\.d
**	ret
*/
TEST_SET (set2_f32_z4_0, svfloat32x2_t, svfloat32_t,
	  z4 = svset2_f32 (z4, 0, z0),
	  z4 = svset2 (z4, 0, z0))

/*
** set2_f32_z4_1:
**	mov	z5\.d, z0\.d
**	ret
*/
TEST_SET (set2_f32_z4_1, svfloat32x2_t, svfloat32_t,
	  z4 = svset2_f32 (z4, 1, z0),
	  z4 = svset2 (z4, 1, z0))
