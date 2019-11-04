/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn2_u32_tied1:
**	trn2	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (trn2_u32_tied1, svuint32_t,
		z0 = svtrn2_u32 (z0, z1),
		z0 = svtrn2 (z0, z1))

/*
** trn2_u32_tied2:
**	trn2	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (trn2_u32_tied2, svuint32_t,
		z0 = svtrn2_u32 (z1, z0),
		z0 = svtrn2 (z1, z0))

/*
** trn2_u32_untied:
**	trn2	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (trn2_u32_untied, svuint32_t,
		z0 = svtrn2_u32 (z1, z2),
		z0 = svtrn2 (z1, z2))
