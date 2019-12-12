/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eor_b_z_tied1:
**	eor	p0\.b, p3/z, (p0\.b, p1\.b|p1\.b, p0\.b)
**	ret
*/
TEST_UNIFORM_P (eor_b_z_tied1,
		p0 = sveor_b_z (p3, p0, p1),
		p0 = sveor_z (p3, p0, p1))

/*
** eor_b_z_tied2:
**	eor	p0\.b, p3/z, (p0\.b, p1\.b|p1\.b, p0\.b)
**	ret
*/
TEST_UNIFORM_P (eor_b_z_tied2,
		p0 = sveor_b_z (p3, p1, p0),
		p0 = sveor_z (p3, p1, p0))

/*
** eor_b_z_untied:
**	eor	p0\.b, p3/z, (p1\.b, p2\.b|p2\.b, p1\.b)
**	ret
*/
TEST_UNIFORM_P (eor_b_z_untied,
		p0 = sveor_b_z (p3, p1, p2),
		p0 = sveor_z (p3, p1, p2))
