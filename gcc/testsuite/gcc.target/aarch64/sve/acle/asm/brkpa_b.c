/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** brkpa_b_z_tied1:
**	brkpa	p0\.b, p3/z, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (brkpa_b_z_tied1,
		p0 = svbrkpa_b_z (p3, p0, p1),
		p0 = svbrkpa_z (p3, p0, p1))

/*
** brkpa_b_z_tied2:
**	brkpa	p0\.b, p3/z, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (brkpa_b_z_tied2,
		p0 = svbrkpa_b_z (p3, p1, p0),
		p0 = svbrkpa_z (p3, p1, p0))

/*
** brkpa_b_z_untied:
**	brkpa	p0\.b, p3/z, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (brkpa_b_z_untied,
		p0 = svbrkpa_b_z (p3, p1, p2),
		p0 = svbrkpa_z (p3, p1, p2))
