/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** not_b_z_tied1:
**	not	p0\.b, p3/z, p0\.b
**	ret
*/
TEST_UNIFORM_P (not_b_z_tied1,
		p0 = svnot_b_z (p3, p0),
		p0 = svnot_z (p3, p0))

/*
** not_b_z_untied:
**	not	p0\.b, p3/z, p1\.b
**	ret
*/
TEST_UNIFORM_P (not_b_z_untied,
		p0 = svnot_b_z (p3, p1),
		p0 = svnot_z (p3, p1))
