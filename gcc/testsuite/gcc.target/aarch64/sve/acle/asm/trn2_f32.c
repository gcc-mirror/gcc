/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn2_f32_tied1:
**	trn2	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (trn2_f32_tied1, svfloat32_t,
		z0 = svtrn2_f32 (z0, z1),
		z0 = svtrn2 (z0, z1))

/*
** trn2_f32_tied2:
**	trn2	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (trn2_f32_tied2, svfloat32_t,
		z0 = svtrn2_f32 (z1, z0),
		z0 = svtrn2 (z1, z0))

/*
** trn2_f32_untied:
**	trn2	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (trn2_f32_untied, svfloat32_t,
		z0 = svtrn2_f32 (z1, z2),
		z0 = svtrn2 (z1, z2))
