/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp1_s64_tied1:
**	uzp1	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (uzp1_s64_tied1, svint64_t,
		z0 = svuzp1_s64 (z0, z1),
		z0 = svuzp1 (z0, z1))

/*
** uzp1_s64_tied2:
**	uzp1	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (uzp1_s64_tied2, svint64_t,
		z0 = svuzp1_s64 (z1, z0),
		z0 = svuzp1 (z1, z0))

/*
** uzp1_s64_untied:
**	uzp1	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (uzp1_s64_untied, svint64_t,
		z0 = svuzp1_s64 (z1, z2),
		z0 = svuzp1 (z1, z2))
