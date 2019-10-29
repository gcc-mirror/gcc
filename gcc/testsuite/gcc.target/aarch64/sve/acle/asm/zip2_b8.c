/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip2_b8_tied1:
**	zip2	p0\.b, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (zip2_b8_tied1,
		p0 = svzip2_b8 (p0, p1),
		p0 = svzip2_b8 (p0, p1))

/*
** zip2_b8_tied2:
**	zip2	p0\.b, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (zip2_b8_tied2,
		p0 = svzip2_b8 (p1, p0),
		p0 = svzip2_b8 (p1, p0))

/*
** zip2_b8_untied:
**	zip2	p0\.b, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (zip2_b8_untied,
		p0 = svzip2_b8 (p1, p2),
		p0 = svzip2_b8 (p1, p2))
