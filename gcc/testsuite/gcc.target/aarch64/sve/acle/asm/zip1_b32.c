/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip1_b32_tied1:
**	zip1	p0\.s, p0\.s, p1\.s
**	ret
*/
TEST_UNIFORM_P (zip1_b32_tied1,
		p0 = svzip1_b32 (p0, p1),
		p0 = svzip1_b32 (p0, p1))

/*
** zip1_b32_tied2:
**	zip1	p0\.s, p1\.s, p0\.s
**	ret
*/
TEST_UNIFORM_P (zip1_b32_tied2,
		p0 = svzip1_b32 (p1, p0),
		p0 = svzip1_b32 (p1, p0))

/*
** zip1_b32_untied:
**	zip1	p0\.s, p1\.s, p2\.s
**	ret
*/
TEST_UNIFORM_P (zip1_b32_untied,
		p0 = svzip1_b32 (p1, p2),
		p0 = svzip1_b32 (p1, p2))
