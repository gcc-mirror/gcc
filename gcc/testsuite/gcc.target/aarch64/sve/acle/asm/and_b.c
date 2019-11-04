/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** and_b_z_tied1:
**	and	p0\.b, p3/z, (p0\.b, p1\.b|p1\.b, p0\.b)
**	ret
*/
TEST_UNIFORM_P (and_b_z_tied1,
		p0 = svand_b_z (p3, p0, p1),
		p0 = svand_z (p3, p0, p1))

/*
** and_b_z_tied2:
**	and	p0\.b, p3/z, (p0\.b, p1\.b|p1\.b, p0\.b)
**	ret
*/
TEST_UNIFORM_P (and_b_z_tied2,
		p0 = svand_b_z (p3, p1, p0),
		p0 = svand_z (p3, p1, p0))

/*
** and_b_z_untied:
**	and	p0\.b, p3/z, (p1\.b, p2\.b|p2\.b, p1\.b)
**	ret
*/
TEST_UNIFORM_P (and_b_z_untied,
		p0 = svand_b_z (p3, p1, p2),
		p0 = svand_z (p3, p1, p2))
