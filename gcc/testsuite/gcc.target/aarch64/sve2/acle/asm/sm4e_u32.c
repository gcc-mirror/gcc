/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-sm4"

/*
** sm4e_u32_tied1:
**	sm4e	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sm4e_u32_tied1, svuint32_t,
		z0 = svsm4e_u32 (z0, z1),
		z0 = svsm4e (z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sm4e_u32_tied2, svuint32_t,
		z0 = svsm4e_u32 (z1, z0),
		z0 = svsm4e (z1, z0))

/*
** sm4e_u32_untied:
** (
**	mov	z0\.d, z1\.d
**	sm4e	z0\.s, z0\.s, z2\.s
** |
**	sm4e	z1\.s, z1\.s, z2\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sm4e_u32_untied, svuint32_t,
		z0 = svsm4e_u32 (z1, z2),
		z0 = svsm4e (z1, z2))
