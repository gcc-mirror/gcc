/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** nor_b_z_tied1:
**	nor	p0\.b, p3/z, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (nor_b_z_tied1,
		p0 = svnor_b_z (p3, p0, p1),
		p0 = svnor_z (p3, p0, p1))

/*
** nor_b_z_tied2:
**	nor	p0\.b, p3/z, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (nor_b_z_tied2,
		p0 = svnor_b_z (p3, p1, p0),
		p0 = svnor_z (p3, p1, p0))

/*
** nor_b_z_untied:
**	nor	p0\.b, p3/z, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (nor_b_z_untied,
		p0 = svnor_b_z (p3, p1, p2),
		p0 = svnor_z (p3, p1, p2))
