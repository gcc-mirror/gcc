/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn1_b8_tied1:
**	trn1	p0\.b, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (trn1_b8_tied1,
		p0 = svtrn1_b8 (p0, p1),
		p0 = svtrn1_b8 (p0, p1))

/*
** trn1_b8_tied2:
**	trn1	p0\.b, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (trn1_b8_tied2,
		p0 = svtrn1_b8 (p1, p0),
		p0 = svtrn1_b8 (p1, p0))

/*
** trn1_b8_untied:
**	trn1	p0\.b, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (trn1_b8_untied,
		p0 = svtrn1_b8 (p1, p2),
		p0 = svtrn1_b8 (p1, p2))
