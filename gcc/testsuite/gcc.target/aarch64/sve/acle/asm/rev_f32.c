/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_f32_tied1:
**	rev	z0\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (rev_f32_tied1, svfloat32_t,
		z0 = svrev_f32 (z0),
		z0 = svrev (z0))

/*
** rev_f32_untied:
**	rev	z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rev_f32_untied, svfloat32_t,
		z0 = svrev_f32 (z1),
		z0 = svrev (z1))
