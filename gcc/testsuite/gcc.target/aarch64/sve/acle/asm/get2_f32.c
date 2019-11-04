/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** get2_f32_z0_0:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_GET (get2_f32_z0_0, svfloat32x2_t, svfloat32_t,
	  z0 = svget2_f32 (z4, 0),
	  z0 = svget2 (z4, 0))

/*
** get2_f32_z0_1:
**	mov	z0\.d, z5\.d
**	ret
*/
TEST_GET (get2_f32_z0_1, svfloat32x2_t, svfloat32_t,
	  z0 = svget2_f32 (z4, 1),
	  z0 = svget2 (z4, 1))

/*
** get2_f32_z4_0:
**	ret
*/
TEST_GET (get2_f32_z4_0, svfloat32x2_t, svfloat32_t,
	  z4_res = svget2_f32 (z4, 0),
	  z4_res = svget2 (z4, 0))

/*
** get2_f32_z4_1:
**	mov	z4\.d, z5\.d
**	ret
*/
TEST_GET (get2_f32_z4_1, svfloat32x2_t, svfloat32_t,
	  z4_res = svget2_f32 (z4, 1),
	  z4_res = svget2 (z4, 1))

/*
** get2_f32_z5_0:
**	mov	z5\.d, z4\.d
**	ret
*/
TEST_GET (get2_f32_z5_0, svfloat32x2_t, svfloat32_t,
	  z5_res = svget2_f32 (z4, 0),
	  z5_res = svget2 (z4, 0))

/*
** get2_f32_z5_1:
**	ret
*/
TEST_GET (get2_f32_z5_1, svfloat32x2_t, svfloat32_t,
	  z5_res = svget2_f32 (z4, 1),
	  z5_res = svget2 (z4, 1))
