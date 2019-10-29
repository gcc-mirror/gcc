/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp2_b16_tied1:
**	uzp2	p0\.h, p0\.h, p1\.h
**	ret
*/
TEST_UNIFORM_P (uzp2_b16_tied1,
		p0 = svuzp2_b16 (p0, p1),
		p0 = svuzp2_b16 (p0, p1))

/*
** uzp2_b16_tied2:
**	uzp2	p0\.h, p1\.h, p0\.h
**	ret
*/
TEST_UNIFORM_P (uzp2_b16_tied2,
		p0 = svuzp2_b16 (p1, p0),
		p0 = svuzp2_b16 (p1, p0))

/*
** uzp2_b16_untied:
**	uzp2	p0\.h, p1\.h, p2\.h
**	ret
*/
TEST_UNIFORM_P (uzp2_b16_untied,
		p0 = svuzp2_b16 (p1, p2),
		p0 = svuzp2_b16 (p1, p2))
