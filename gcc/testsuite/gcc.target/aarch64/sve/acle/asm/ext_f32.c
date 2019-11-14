/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ext_0_f32_tied1:
**	ext	z0\.b, z0\.b, z1\.b, #0
**	ret
*/
TEST_UNIFORM_Z (ext_0_f32_tied1, svfloat32_t,
		z0 = svext_f32 (z0, z1, 0),
		z0 = svext (z0, z1, 0))

/*
** ext_0_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, \1\.b, #0
**	ret
*/
TEST_UNIFORM_Z (ext_0_f32_tied2, svfloat32_t,
		z0 = svext_f32 (z1, z0, 0),
		z0 = svext (z1, z0, 0))

/*
** ext_0_f32_untied:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #0
**	ret
*/
TEST_UNIFORM_Z (ext_0_f32_untied, svfloat32_t,
		z0 = svext_f32 (z1, z2, 0),
		z0 = svext (z1, z2, 0))

/*
** ext_1_f32:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #4
**	ret
*/
TEST_UNIFORM_Z (ext_1_f32, svfloat32_t,
		z0 = svext_f32 (z1, z2, 1),
		z0 = svext (z1, z2, 1))

/*
** ext_2_f32:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #8
**	ret
*/
TEST_UNIFORM_Z (ext_2_f32, svfloat32_t,
		z0 = svext_f32 (z1, z2, 2),
		z0 = svext (z1, z2, 2))

/*
** ext_3_f32:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #12
**	ret
*/
TEST_UNIFORM_Z (ext_3_f32, svfloat32_t,
		z0 = svext_f32 (z1, z2, 3),
		z0 = svext (z1, z2, 3))

/*
** ext_63_f32:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #252
**	ret
*/
TEST_UNIFORM_Z (ext_63_f32, svfloat32_t,
		z0 = svext_f32 (z1, z2, 63),
		z0 = svext (z1, z2, 63))
