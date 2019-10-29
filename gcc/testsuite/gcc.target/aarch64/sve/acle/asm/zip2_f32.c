/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip2_f32_tied1:
**	zip2	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (zip2_f32_tied1, svfloat32_t,
		z0 = svzip2_f32 (z0, z1),
		z0 = svzip2 (z0, z1))

/*
** zip2_f32_tied2:
**	zip2	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (zip2_f32_tied2, svfloat32_t,
		z0 = svzip2_f32 (z1, z0),
		z0 = svzip2 (z1, z0))

/*
** zip2_f32_untied:
**	zip2	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (zip2_f32_untied, svfloat32_t,
		z0 = svzip2_f32 (z1, z2),
		z0 = svzip2 (z1, z2))
