/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** trn2_b32_tied1:
**	trn2	p0\.s, p0\.s, p1\.s
**	ret
*/
TEST_UNIFORM_P (trn2_b32_tied1,
		p0 = svtrn2_b32 (p0, p1),
		p0 = svtrn2_b32 (p0, p1))

/*
** trn2_b32_tied2:
**	trn2	p0\.s, p1\.s, p0\.s
**	ret
*/
TEST_UNIFORM_P (trn2_b32_tied2,
		p0 = svtrn2_b32 (p1, p0),
		p0 = svtrn2_b32 (p1, p0))

/*
** trn2_b32_untied:
**	trn2	p0\.s, p1\.s, p2\.s
**	ret
*/
TEST_UNIFORM_P (trn2_b32_untied,
		p0 = svtrn2_b32 (p1, p2),
		p0 = svtrn2_b32 (p1, p2))
