/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip2_b64_tied1:
**	zip2	p0\.d, p0\.d, p1\.d
**	ret
*/
TEST_UNIFORM_P (zip2_b64_tied1,
		p0 = svzip2_b64 (p0, p1),
		p0 = svzip2_b64 (p0, p1))

/*
** zip2_b64_tied2:
**	zip2	p0\.d, p1\.d, p0\.d
**	ret
*/
TEST_UNIFORM_P (zip2_b64_tied2,
		p0 = svzip2_b64 (p1, p0),
		p0 = svzip2_b64 (p1, p0))

/*
** zip2_b64_untied:
**	zip2	p0\.d, p1\.d, p2\.d
**	ret
*/
TEST_UNIFORM_P (zip2_b64_untied,
		p0 = svzip2_b64 (p1, p2),
		p0 = svzip2_b64 (p1, p2))
