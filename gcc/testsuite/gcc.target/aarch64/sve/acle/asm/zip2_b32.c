/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip2_b32_tied1:
**	zip2	p0\.s, p0\.s, p1\.s
**	ret
*/
TEST_UNIFORM_P (zip2_b32_tied1,
		p0 = svzip2_b32 (p0, p1),
		p0 = svzip2_b32 (p0, p1))

/*
** zip2_b32_tied2:
**	zip2	p0\.s, p1\.s, p0\.s
**	ret
*/
TEST_UNIFORM_P (zip2_b32_tied2,
		p0 = svzip2_b32 (p1, p0),
		p0 = svzip2_b32 (p1, p0))

/*
** zip2_b32_untied:
**	zip2	p0\.s, p1\.s, p2\.s
**	ret
*/
TEST_UNIFORM_P (zip2_b32_untied,
		p0 = svzip2_b32 (p1, p2),
		p0 = svzip2_b32 (p1, p2))
