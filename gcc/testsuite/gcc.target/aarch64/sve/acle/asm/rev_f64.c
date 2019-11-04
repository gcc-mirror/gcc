/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_f64_tied1:
**	rev	z0\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (rev_f64_tied1, svfloat64_t,
		z0 = svrev_f64 (z0),
		z0 = svrev (z0))

/*
** rev_f64_untied:
**	rev	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rev_f64_untied, svfloat64_t,
		z0 = svrev_f64 (z1),
		z0 = svrev (z1))
