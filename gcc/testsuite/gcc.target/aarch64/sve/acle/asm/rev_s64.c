/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_s64_tied1:
**	rev	z0\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (rev_s64_tied1, svint64_t,
		z0 = svrev_s64 (z0),
		z0 = svrev (z0))

/*
** rev_s64_untied:
**	rev	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rev_s64_untied, svint64_t,
		z0 = svrev_s64 (z1),
		z0 = svrev (z1))
