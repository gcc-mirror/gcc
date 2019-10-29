/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_s32_tied1:
**	rev	z0\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (rev_s32_tied1, svint32_t,
		z0 = svrev_s32 (z0),
		z0 = svrev (z0))

/*
** rev_s32_untied:
**	rev	z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rev_s32_untied, svint32_t,
		z0 = svrev_s32 (z1),
		z0 = svrev (z1))
