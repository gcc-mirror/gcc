/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sel_f32_tied1:
**	sel	z0\.s, p0, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sel_f32_tied1, svfloat32_t,
		z0 = svsel_f32 (p0, z0, z1),
		z0 = svsel (p0, z0, z1))

/*
** sel_f32_tied2:
**	sel	z0\.s, p0, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (sel_f32_tied2, svfloat32_t,
		z0 = svsel_f32 (p0, z1, z0),
		z0 = svsel (p0, z1, z0))

/*
** sel_f32_untied:
**	sel	z0\.s, p0, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (sel_f32_untied, svfloat32_t,
		z0 = svsel_f32 (p0, z1, z2),
		z0 = svsel (p0, z1, z2))
