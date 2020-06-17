/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-aes"

/*
** aesd_u8_tied1:
**	aesd	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (aesd_u8_tied1, svuint8_t,
		z0 = svaesd_u8 (z0, z1),
		z0 = svaesd (z0, z1))

/*
** aesd_u8_tied2:
**	aesd	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (aesd_u8_tied2, svuint8_t,
		z0 = svaesd_u8 (z1, z0),
		z0 = svaesd (z1, z0))

/*
** aesd_u8_untied:
** (
**	mov	z0\.d, z1\.d
**	aesd	z0\.b, z0\.b, z2\.b
** |
**	aesd	z1\.b, z0\.b, z2\.b
**	mov	z0\.d, z1\.d
** |
**	mov	z0\.d, z2\.d
**	aesd	z0\.b, z0\.b, z1\.b
** |
**	aesd	z2\.b, z0\.b, z1\.b
**	mov	z0\.d, z2\.d
** )
**	ret
*/
TEST_UNIFORM_Z (aesd_u8_untied, svuint8_t,
		z0 = svaesd_u8 (z1, z2),
		z0 = svaesd (z1, z2))
