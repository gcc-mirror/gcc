/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** orr_b_z_tied1:
**	orr	p0\.b, p3/z, (p0\.b, p1\.b|p1\.b, p0\.b)
**	ret
*/
TEST_UNIFORM_P (orr_b_z_tied1,
		p0 = svorr_b_z (p3, p0, p1),
		p0 = svorr_z (p3, p0, p1))

/*
** orr_b_z_tied2:
**	orr	p0\.b, p3/z, (p0\.b, p1\.b|p1\.b, p0\.b)
**	ret
*/
TEST_UNIFORM_P (orr_b_z_tied2,
		p0 = svorr_b_z (p3, p1, p0),
		p0 = svorr_z (p3, p1, p0))

/*
** orr_b_z_untied:
**	orr	p0\.b, p3/z, (p1\.b, p2\.b|p2\.b, p1\.b)
**	ret
*/
TEST_UNIFORM_P (orr_b_z_untied,
		p0 = svorr_b_z (p3, p1, p2),
		p0 = svorr_z (p3, p1, p2))
