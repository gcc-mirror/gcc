/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cadd_90_u8_tied1:
**	cadd	z0\.b, z0\.b, z1\.b, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_u8_tied1, svuint8_t,
		z0 = svcadd_u8 (z0, z1, 90),
		z0 = svcadd (z0, z1, 90))

/*
** cadd_90_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cadd	z0\.b, z0\.b, \1\.b, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_u8_tied2, svuint8_t,
		z0 = svcadd_u8 (z1, z0, 90),
		z0 = svcadd (z1, z0, 90))

/*
** cadd_90_u8_untied:
**	movprfx	z0, z1
**	cadd	z0\.b, z0\.b, z2\.b, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_u8_untied, svuint8_t,
		z0 = svcadd_u8 (z1, z2, 90),
		z0 = svcadd (z1, z2, 90))

/*
** cadd_270_u8_tied1:
**	cadd	z0\.b, z0\.b, z1\.b, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_u8_tied1, svuint8_t,
		z0 = svcadd_u8 (z0, z1, 270),
		z0 = svcadd (z0, z1, 270))

/*
** cadd_270_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cadd	z0\.b, z0\.b, \1\.b, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_u8_tied2, svuint8_t,
		z0 = svcadd_u8 (z1, z0, 270),
		z0 = svcadd (z1, z0, 270))

/*
** cadd_270_u8_untied:
**	movprfx	z0, z1
**	cadd	z0\.b, z0\.b, z2\.b, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_u8_untied, svuint8_t,
		z0 = svcadd_u8 (z1, z2, 270),
		z0 = svcadd (z1, z2, 270))
