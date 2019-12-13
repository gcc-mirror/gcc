/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp2_b8_tied1:
**	uzp2	p0\.b, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (uzp2_b8_tied1,
		p0 = svuzp2_b8 (p0, p1),
		p0 = svuzp2_b8 (p0, p1))

/*
** uzp2_b8_tied2:
**	uzp2	p0\.b, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (uzp2_b8_tied2,
		p0 = svuzp2_b8 (p1, p0),
		p0 = svuzp2_b8 (p1, p0))

/*
** uzp2_b8_untied:
**	uzp2	p0\.b, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (uzp2_b8_untied,
		p0 = svuzp2_b8 (p1, p2),
		p0 = svuzp2_b8 (p1, p2))
