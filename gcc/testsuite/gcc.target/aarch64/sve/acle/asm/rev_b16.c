/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_b16_tied1:
**	rev	p0\.h, p0\.h
**	ret
*/
TEST_UNIFORM_P (rev_b16_tied1,
		p0 = svrev_b16 (p0),
		p0 = svrev_b16 (p0))

/*
** rev_b16_untied:
**	rev	p0\.h, p1\.h
**	ret
*/
TEST_UNIFORM_P (rev_b16_untied,
		p0 = svrev_b16 (p1),
		p0 = svrev_b16 (p1))
