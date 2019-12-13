/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsqrts_f32_tied1:
**	frsqrts	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rsqrts_f32_tied1, svfloat32_t,
		z0 = svrsqrts_f32 (z0, z1),
		z0 = svrsqrts (z0, z1))

/*
** rsqrts_f32_tied2:
**	frsqrts	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (rsqrts_f32_tied2, svfloat32_t,
		z0 = svrsqrts_f32 (z1, z0),
		z0 = svrsqrts (z1, z0))

/*
** rsqrts_f32_untied:
**	frsqrts	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (rsqrts_f32_untied, svfloat32_t,
		z0 = svrsqrts_f32 (z1, z2),
		z0 = svrsqrts (z1, z2))
