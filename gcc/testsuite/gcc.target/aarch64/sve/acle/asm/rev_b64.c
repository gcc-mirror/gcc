/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_b64_tied1:
**	rev	p0\.d, p0\.d
**	ret
*/
TEST_UNIFORM_P (rev_b64_tied1,
		p0 = svrev_b64 (p0),
		p0 = svrev_b64 (p0))

/*
** rev_b64_untied:
**	rev	p0\.d, p1\.d
**	ret
*/
TEST_UNIFORM_P (rev_b64_untied,
		p0 = svrev_b64 (p1),
		p0 = svrev_b64 (p1))
