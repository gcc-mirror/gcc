/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** compact_u64_tied1:
**	compact	z0\.d, p0, z0\.d
**	ret
*/
TEST_UNIFORM_Z (compact_u64_tied1, svuint64_t,
		z0 = svcompact_u64 (p0, z0),
		z0 = svcompact (p0, z0))

/*
** compact_u64_untied:
**	compact	z0\.d, p0, z1\.d
**	ret
*/
TEST_UNIFORM_Z (compact_u64_untied, svuint64_t,
		z0 = svcompact_u64 (p0, z1),
		z0 = svcompact (p0, z1))
