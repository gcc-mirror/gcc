/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bic_b_z_tied1:
**	bic	p0\.b, p3/z, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (bic_b_z_tied1,
		p0 = svbic_b_z (p3, p0, p1),
		p0 = svbic_z (p3, p0, p1))

/*
** bic_b_z_tied2:
**	bic	p0\.b, p3/z, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (bic_b_z_tied2,
		p0 = svbic_b_z (p3, p1, p0),
		p0 = svbic_z (p3, p1, p0))

/*
** bic_b_z_untied:
**	bic	p0\.b, p3/z, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (bic_b_z_untied,
		p0 = svbic_b_z (p3, p1, p2),
		p0 = svbic_z (p3, p1, p2))
