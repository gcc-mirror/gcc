/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_b8_tied1:
**	rev	p0\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (rev_b8_tied1,
		p0 = svrev_b8 (p0),
		p0 = svrev_b8 (p0))

/*
** rev_b8_untied:
**	rev	p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (rev_b8_untied,
		p0 = svrev_b8 (p1),
		p0 = svrev_b8 (p1))
