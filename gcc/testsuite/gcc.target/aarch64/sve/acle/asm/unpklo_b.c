/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpklo_b_tied1:
**	punpklo	p0\.h, p0\.b
**	ret
*/
TEST_UNIFORM_P (unpklo_b_tied1,
		p0 = svunpklo_b (p0),
		p0 = svunpklo (p0))

/*
** unpklo_b_untied:
**	punpklo	p0\.h, p1\.b
**	ret
*/
TEST_UNIFORM_P (unpklo_b_untied,
		p0 = svunpklo_b (p1),
		p0 = svunpklo (p1))
