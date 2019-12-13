/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_b32_tied1:
**	rev	p0\.s, p0\.s
**	ret
*/
TEST_UNIFORM_P (rev_b32_tied1,
		p0 = svrev_b32 (p0),
		p0 = svrev_b32 (p0))

/*
** rev_b32_untied:
**	rev	p0\.s, p1\.s
**	ret
*/
TEST_UNIFORM_P (rev_b32_untied,
		p0 = svrev_b32 (p1),
		p0 = svrev_b32 (p1))
