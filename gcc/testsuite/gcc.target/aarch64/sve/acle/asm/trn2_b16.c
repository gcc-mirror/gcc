/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn2_b16_tied1:
**	trn2	p0\.h, p0\.h, p1\.h
**	ret
*/
TEST_UNIFORM_P (trn2_b16_tied1,
		p0 = svtrn2_b16 (p0, p1),
		p0 = svtrn2_b16 (p0, p1))

/*
** trn2_b16_tied2:
**	trn2	p0\.h, p1\.h, p0\.h
**	ret
*/
TEST_UNIFORM_P (trn2_b16_tied2,
		p0 = svtrn2_b16 (p1, p0),
		p0 = svtrn2_b16 (p1, p0))

/*
** trn2_b16_untied:
**	trn2	p0\.h, p1\.h, p2\.h
**	ret
*/
TEST_UNIFORM_P (trn2_b16_untied,
		p0 = svtrn2_b16 (p1, p2),
		p0 = svtrn2_b16 (p1, p2))
