/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp2_s8_tied1:
**	uzp2	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (uzp2_s8_tied1, svint8_t,
		z0 = svuzp2_s8 (z0, z1),
		z0 = svuzp2 (z0, z1))

/*
** uzp2_s8_tied2:
**	uzp2	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (uzp2_s8_tied2, svint8_t,
		z0 = svuzp2_s8 (z1, z0),
		z0 = svuzp2 (z1, z0))

/*
** uzp2_s8_untied:
**	uzp2	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (uzp2_s8_untied, svint8_t,
		z0 = svuzp2_s8 (z1, z2),
		z0 = svuzp2 (z1, z2))
