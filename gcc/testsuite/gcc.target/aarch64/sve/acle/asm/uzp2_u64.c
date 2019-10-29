/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp2_u64_tied1:
**	uzp2	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (uzp2_u64_tied1, svuint64_t,
		z0 = svuzp2_u64 (z0, z1),
		z0 = svuzp2 (z0, z1))

/*
** uzp2_u64_tied2:
**	uzp2	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (uzp2_u64_tied2, svuint64_t,
		z0 = svuzp2_u64 (z1, z0),
		z0 = svuzp2 (z1, z0))

/*
** uzp2_u64_untied:
**	uzp2	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (uzp2_u64_untied, svuint64_t,
		z0 = svuzp2_u64 (z1, z2),
		z0 = svuzp2 (z1, z2))
