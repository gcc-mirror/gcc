/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mov_b_z_tied1:
**	and	p0\.b, (?:p3/z, p0\.b, p0\.b|p0/z, p3\.b, p3\.b)
**	ret
*/
TEST_UNIFORM_P (mov_b_z_tied1,
		p0 = svmov_b_z (p3, p0),
		p0 = svmov_z (p3, p0))

/*
** mov_b_z_untied:
**	and	p0\.b, (?:p3/z, p1\.b, p1\.b|p1/z, p3\.b, p3\.b)
**	ret
*/
TEST_UNIFORM_P (mov_b_z_untied,
		p0 = svmov_b_z (p3, p1),
		p0 = svmov_z (p3, p1))
