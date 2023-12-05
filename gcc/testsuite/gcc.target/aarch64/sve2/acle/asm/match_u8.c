/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** match_u8_tied:
**	match	p0\.b, p0/z, z0\.b, z1\.b
**	ret
*/
TEST_COMPARE_Z (match_u8_tied, svuint8_t,
		p0 = svmatch_u8 (p0, z0, z1),
		p0 = svmatch (p0, z0, z1))

/*
** match_u8_untied:
**	match	p0\.b, p1/z, z0\.b, z1\.b
**	ret
*/
TEST_COMPARE_Z (match_u8_untied, svuint8_t,
		p0 = svmatch_u8 (p1, z0, z1),
		p0 = svmatch (p1, z0, z1))
