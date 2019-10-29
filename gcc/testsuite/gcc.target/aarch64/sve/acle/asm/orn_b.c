/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** orn_b_z_tied1:
**	orn	p0\.b, p3/z, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (orn_b_z_tied1,
		p0 = svorn_b_z (p3, p0, p1),
		p0 = svorn_z (p3, p0, p1))

/*
** orn_b_z_tied2:
**	orn	p0\.b, p3/z, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (orn_b_z_tied2,
		p0 = svorn_b_z (p3, p1, p0),
		p0 = svorn_z (p3, p1, p0))

/*
** orn_b_z_untied:
**	orn	p0\.b, p3/z, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (orn_b_z_untied,
		p0 = svorn_b_z (p3, p1, p2),
		p0 = svorn_z (p3, p1, p2))
