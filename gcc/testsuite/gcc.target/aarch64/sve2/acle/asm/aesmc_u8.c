/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-aes"

/*
** aesmc_u8_tied1:
**	aesmc	z0\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (aesmc_u8_tied1, svuint8_t,
		z0 = svaesmc_u8 (z0),
		z0 = svaesmc (z0))

/*
** aesmc_u8_untied:
** (
**	mov	z0\.d, z1\.d
**	aesmc	z0\.b, z0\.b
** |
**	aesmc	z1\.b, z0\.b
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (aesmc_u8_untied, svuint8_t,
		z0 = svaesmc_u8 (z1),
		z0 = svaesmc (z1))
