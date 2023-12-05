/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-sm4"

/*
** sm4ekey_u32_tied1:
**	sm4ekey	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sm4ekey_u32_tied1, svuint32_t,
		z0 = svsm4ekey_u32 (z0, z1),
		z0 = svsm4ekey (z0, z1))

/*
** sm4ekey_u32_tied2:
**	sm4ekey	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (sm4ekey_u32_tied2, svuint32_t,
		z0 = svsm4ekey_u32 (z1, z0),
		z0 = svsm4ekey (z1, z0))

/*
** sm4ekey_u32_untied:
**	sm4ekey	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (sm4ekey_u32_untied, svuint32_t,
		z0 = svsm4ekey_u32 (z1, z2),
		z0 = svsm4ekey (z1, z2))
