/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp2_f32_tied1:
**	uzp2	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (uzp2_f32_tied1, svfloat32_t,
		z0 = svuzp2_f32 (z0, z1),
		z0 = svuzp2 (z0, z1))

/*
** uzp2_f32_tied2:
**	uzp2	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (uzp2_f32_tied2, svfloat32_t,
		z0 = svuzp2_f32 (z1, z0),
		z0 = svuzp2 (z1, z0))

/*
** uzp2_f32_untied:
**	uzp2	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (uzp2_f32_untied, svfloat32_t,
		z0 = svuzp2_f32 (z1, z2),
		z0 = svuzp2 (z1, z2))
