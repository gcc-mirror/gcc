/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** nmatch_u16_tied:
**	nmatch	p0\.h, p0/z, z0\.h, z1\.h
**	ret
*/
TEST_COMPARE_Z (nmatch_u16_tied, svuint16_t,
		p0 = svnmatch_u16 (p0, z0, z1),
		p0 = svnmatch (p0, z0, z1))

/*
** nmatch_u16_untied:
**	nmatch	p0\.h, p1/z, z0\.h, z1\.h
**	ret
*/
TEST_COMPARE_Z (nmatch_u16_untied, svuint16_t,
		p0 = svnmatch_u16 (p1, z0, z1),
		p0 = svnmatch (p1, z0, z1))
