/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** match_s16_tied:
**	match	p0\.h, p0/z, z0\.h, z1\.h
**	ret
*/
TEST_COMPARE_Z (match_s16_tied, svint16_t,
		p0 = svmatch_s16 (p0, z0, z1),
		p0 = svmatch (p0, z0, z1))

/*
** match_s16_untied:
**	match	p0\.h, p1/z, z0\.h, z1\.h
**	ret
*/
TEST_COMPARE_Z (match_s16_untied, svint16_t,
		p0 = svmatch_s16 (p1, z0, z1),
		p0 = svmatch (p1, z0, z1))
