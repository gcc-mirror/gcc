/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-aes"

/*
** aese_u8_tied1:
**	aese	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (aese_u8_tied1, svuint8_t,
		z0 = svaese_u8 (z0, z1),
		z0 = svaese (z0, z1))

/*
** aese_u8_tied2:
**	aese	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (aese_u8_tied2, svuint8_t,
		z0 = svaese_u8 (z1, z0),
		z0 = svaese (z1, z0))

/*
** aese_u8_untied:
** (
**	mov	z0\.d, z1\.d
**	aese	z0\.b, z0\.b, z2\.b
** |
**	aese	z1\.b, z1\.b, z2\.b
**	mov	z0\.d, z1\.d
** |
**	mov	z0\.d, z2\.d
**	aese	z0\.b, z0\.b, z1\.b
** |
**	aese	z2\.b, z2\.b, z1\.b
**	mov	z0\.d, z2\.d
** )
**	ret
*/
TEST_UNIFORM_Z (aese_u8_untied, svuint8_t,
		z0 = svaese_u8 (z1, z2),
		z0 = svaese (z1, z2))
