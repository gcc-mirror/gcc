/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_s16_tied1:
**	rev	z0\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rev_s16_tied1, svint16_t,
		z0 = svrev_s16 (z0),
		z0 = svrev (z0))

/*
** rev_s16_untied:
**	rev	z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rev_s16_untied, svint16_t,
		z0 = svrev_s16 (z1),
		z0 = svrev (z1))
