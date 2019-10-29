/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_u32_tied1:
**	rev	z0\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (rev_u32_tied1, svuint32_t,
		z0 = svrev_u32 (z0),
		z0 = svrev (z0))

/*
** rev_u32_untied:
**	rev	z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rev_u32_untied, svuint32_t,
		z0 = svrev_u32 (z1),
		z0 = svrev (z1))
