/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn1_b16_tied1:
**	trn1	p0\.h, p0\.h, p1\.h
**	ret
*/
TEST_UNIFORM_P (trn1_b16_tied1,
		p0 = svtrn1_b16 (p0, p1),
		p0 = svtrn1_b16 (p0, p1))

/*
** trn1_b16_tied2:
**	trn1	p0\.h, p1\.h, p0\.h
**	ret
*/
TEST_UNIFORM_P (trn1_b16_tied2,
		p0 = svtrn1_b16 (p1, p0),
		p0 = svtrn1_b16 (p1, p0))

/*
** trn1_b16_untied:
**	trn1	p0\.h, p1\.h, p2\.h
**	ret
*/
TEST_UNIFORM_P (trn1_b16_untied,
		p0 = svtrn1_b16 (p1, p2),
		p0 = svtrn1_b16 (p1, p2))
