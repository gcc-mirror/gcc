/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** uzp1_b32_tied1:
**	uzp1	p0\.s, p0\.s, p1\.s
**	ret
*/
TEST_UNIFORM_P (uzp1_b32_tied1,
		p0 = svuzp1_b32 (p0, p1),
		p0 = svuzp1_b32 (p0, p1))

/*
** uzp1_b32_tied2:
**	uzp1	p0\.s, p1\.s, p0\.s
**	ret
*/
TEST_UNIFORM_P (uzp1_b32_tied2,
		p0 = svuzp1_b32 (p1, p0),
		p0 = svuzp1_b32 (p1, p0))

/*
** uzp1_b32_untied:
**	uzp1	p0\.s, p1\.s, p2\.s
**	ret
*/
TEST_UNIFORM_P (uzp1_b32_untied,
		p0 = svuzp1_b32 (p1, p2),
		p0 = svuzp1_b32 (p1, p2))
