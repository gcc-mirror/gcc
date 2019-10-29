/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp2_b64_tied1:
**	uzp2	p0\.d, p0\.d, p1\.d
**	ret
*/
TEST_UNIFORM_P (uzp2_b64_tied1,
		p0 = svuzp2_b64 (p0, p1),
		p0 = svuzp2_b64 (p0, p1))

/*
** uzp2_b64_tied2:
**	uzp2	p0\.d, p1\.d, p0\.d
**	ret
*/
TEST_UNIFORM_P (uzp2_b64_tied2,
		p0 = svuzp2_b64 (p1, p0),
		p0 = svuzp2_b64 (p1, p0))

/*
** uzp2_b64_untied:
**	uzp2	p0\.d, p1\.d, p2\.d
**	ret
*/
TEST_UNIFORM_P (uzp2_b64_untied,
		p0 = svuzp2_b64 (p1, p2),
		p0 = svuzp2_b64 (p1, p2))
