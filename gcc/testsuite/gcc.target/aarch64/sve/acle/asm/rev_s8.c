/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_s8_tied1:
**	rev	z0\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (rev_s8_tied1, svint8_t,
		z0 = svrev_s8 (z0),
		z0 = svrev (z0))

/*
** rev_s8_untied:
**	rev	z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (rev_s8_untied, svint8_t,
		z0 = svrev_s8 (z1),
		z0 = svrev (z1))
