/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_bf16_tied1:
**	rev	z0\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rev_bf16_tied1, svbfloat16_t,
		z0 = svrev_bf16 (z0),
		z0 = svrev (z0))

/*
** rev_bf16_untied:
**	rev	z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rev_bf16_untied, svbfloat16_t,
		z0 = svrev_bf16 (z1),
		z0 = svrev (z1))
