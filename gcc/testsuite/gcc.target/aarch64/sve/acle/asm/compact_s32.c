/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** compact_s32_tied1:
**	compact	z0\.s, p0, z0\.s
**	ret
*/
TEST_UNIFORM_Z (compact_s32_tied1, svint32_t,
		z0 = svcompact_s32 (p0, z0),
		z0 = svcompact (p0, z0))

/*
** compact_s32_untied:
**	compact	z0\.s, p0, z1\.s
**	ret
*/
TEST_UNIFORM_Z (compact_s32_untied, svint32_t,
		z0 = svcompact_s32 (p0, z1),
		z0 = svcompact (p0, z1))
