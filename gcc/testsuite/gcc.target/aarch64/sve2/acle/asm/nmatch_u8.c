/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** nmatch_u8_tied:
**	nmatch	p0\.b, p0/z, z0\.b, z1\.b
**	ret
*/
TEST_COMPARE_Z (nmatch_u8_tied, svuint8_t,
		p0 = svnmatch_u8 (p0, z0, z1),
		p0 = svnmatch (p0, z0, z1))

/*
** nmatch_u8_untied:
**	nmatch	p0\.b, p1/z, z0\.b, z1\.b
**	ret
*/
TEST_COMPARE_Z (nmatch_u8_untied, svuint8_t,
		p0 = svnmatch_u8 (p1, z0, z1),
		p0 = svnmatch (p1, z0, z1))
