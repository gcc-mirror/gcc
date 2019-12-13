/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn2_b8_tied1:
**	trn2	p0\.b, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (trn2_b8_tied1,
		p0 = svtrn2_b8 (p0, p1),
		p0 = svtrn2_b8 (p0, p1))

/*
** trn2_b8_tied2:
**	trn2	p0\.b, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (trn2_b8_tied2,
		p0 = svtrn2_b8 (p1, p0),
		p0 = svtrn2_b8 (p1, p0))

/*
** trn2_b8_untied:
**	trn2	p0\.b, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (trn2_b8_untied,
		p0 = svtrn2_b8 (p1, p2),
		p0 = svtrn2_b8 (p1, p2))
