/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_f16_tied1:
**	rev	z0\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rev_f16_tied1, svfloat16_t,
		z0 = svrev_f16 (z0),
		z0 = svrev (z0))

/*
** rev_f16_untied:
**	rev	z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rev_f16_untied, svfloat16_t,
		z0 = svrev_f16 (z1),
		z0 = svrev (z1))
