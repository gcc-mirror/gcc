/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpkhi_b_tied1:
**	punpkhi	p0\.h, p0\.b
**	ret
*/
TEST_UNIFORM_P (unpkhi_b_tied1,
		p0 = svunpkhi_b (p0),
		p0 = svunpkhi (p0))

/*
** unpkhi_b_untied:
**	punpkhi	p0\.h, p1\.b
**	ret
*/
TEST_UNIFORM_P (unpkhi_b_untied,
		p0 = svunpkhi_b (p1),
		p0 = svunpkhi (p1))
