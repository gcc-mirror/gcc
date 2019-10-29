/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** compact_u32_tied1:
**	compact	z0\.s, p0, z0\.s
**	ret
*/
TEST_UNIFORM_Z (compact_u32_tied1, svuint32_t,
		z0 = svcompact_u32 (p0, z0),
		z0 = svcompact (p0, z0))

/*
** compact_u32_untied:
**	compact	z0\.s, p0, z1\.s
**	ret
*/
TEST_UNIFORM_Z (compact_u32_untied, svuint32_t,
		z0 = svcompact_u32 (p0, z1),
		z0 = svcompact (p0, z1))
