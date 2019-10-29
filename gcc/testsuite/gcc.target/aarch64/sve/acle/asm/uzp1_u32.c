/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp1_u32_tied1:
**	uzp1	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (uzp1_u32_tied1, svuint32_t,
		z0 = svuzp1_u32 (z0, z1),
		z0 = svuzp1 (z0, z1))

/*
** uzp1_u32_tied2:
**	uzp1	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (uzp1_u32_tied2, svuint32_t,
		z0 = svuzp1_u32 (z1, z0),
		z0 = svuzp1 (z1, z0))

/*
** uzp1_u32_untied:
**	uzp1	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (uzp1_u32_untied, svuint32_t,
		z0 = svuzp1_u32 (z1, z2),
		z0 = svuzp1 (z1, z2))
