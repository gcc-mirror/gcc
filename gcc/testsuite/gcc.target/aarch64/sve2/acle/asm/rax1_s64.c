/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-sha3"

/*
** rax1_s64_tied1:
**	rax1	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rax1_s64_tied1, svint64_t,
		z0 = svrax1_s64 (z0, z1),
		z0 = svrax1 (z0, z1))

/*
** rax1_s64_tied2:
**	rax1	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (rax1_s64_tied2, svint64_t,
		z0 = svrax1_s64 (z1, z0),
		z0 = svrax1 (z1, z0))

/*
** rax1_s64_untied:
**	rax1	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (rax1_s64_untied, svint64_t,
		z0 = svrax1_s64 (z1, z2),
		z0 = svrax1 (z1, z2))
