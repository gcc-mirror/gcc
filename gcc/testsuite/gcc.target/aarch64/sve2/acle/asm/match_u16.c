/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** match_u16_tied:
**	match	p0\.h, p0/z, z0\.h, z1\.h
**	ret
*/
TEST_COMPARE_Z (match_u16_tied, svuint16_t,
		p0 = svmatch_u16 (p0, z0, z1),
		p0 = svmatch (p0, z0, z1))

/*
** match_u16_untied:
**	match	p0\.h, p1/z, z0\.h, z1\.h
**	ret
*/
TEST_COMPARE_Z (match_u16_untied, svuint16_t,
		p0 = svmatch_u16 (p1, z0, z1),
		p0 = svmatch (p1, z0, z1))
