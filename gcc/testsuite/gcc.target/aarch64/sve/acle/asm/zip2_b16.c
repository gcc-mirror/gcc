/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip2_b16_tied1:
**	zip2	p0\.h, p0\.h, p1\.h
**	ret
*/
TEST_UNIFORM_P (zip2_b16_tied1,
		p0 = svzip2_b16 (p0, p1),
		p0 = svzip2_b16 (p0, p1))

/*
** zip2_b16_tied2:
**	zip2	p0\.h, p1\.h, p0\.h
**	ret
*/
TEST_UNIFORM_P (zip2_b16_tied2,
		p0 = svzip2_b16 (p1, p0),
		p0 = svzip2_b16 (p1, p0))

/*
** zip2_b16_untied:
**	zip2	p0\.h, p1\.h, p2\.h
**	ret
*/
TEST_UNIFORM_P (zip2_b16_untied,
		p0 = svzip2_b16 (p1, p2),
		p0 = svzip2_b16 (p1, p2))
