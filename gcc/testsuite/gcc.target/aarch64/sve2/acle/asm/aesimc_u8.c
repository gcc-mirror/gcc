/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-aes"

/*
** aesimc_u8_tied1:
**	aesimc	z0\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (aesimc_u8_tied1, svuint8_t,
		z0 = svaesimc_u8 (z0),
		z0 = svaesimc (z0))

/*
** aesimc_u8_untied:
** (
**	mov	z0\.d, z1\.d
**	aesimc	z0\.b, z0\.b
** |
**	aesimc	z1\.b, z0\.b
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (aesimc_u8_untied, svuint8_t,
		z0 = svaesimc_u8 (z1),
		z0 = svaesimc (z1))
