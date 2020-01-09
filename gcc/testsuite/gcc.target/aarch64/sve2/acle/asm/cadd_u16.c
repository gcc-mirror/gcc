/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cadd_90_u16_tied1:
**	cadd	z0\.h, z0\.h, z1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_u16_tied1, svuint16_t,
		z0 = svcadd_u16 (z0, z1, 90),
		z0 = svcadd (z0, z1, 90))

/*
** cadd_90_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cadd	z0\.h, z0\.h, \1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_u16_tied2, svuint16_t,
		z0 = svcadd_u16 (z1, z0, 90),
		z0 = svcadd (z1, z0, 90))

/*
** cadd_90_u16_untied:
**	movprfx	z0, z1
**	cadd	z0\.h, z0\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_u16_untied, svuint16_t,
		z0 = svcadd_u16 (z1, z2, 90),
		z0 = svcadd (z1, z2, 90))

/*
** cadd_270_u16_tied1:
**	cadd	z0\.h, z0\.h, z1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_u16_tied1, svuint16_t,
		z0 = svcadd_u16 (z0, z1, 270),
		z0 = svcadd (z0, z1, 270))

/*
** cadd_270_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cadd	z0\.h, z0\.h, \1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_u16_tied2, svuint16_t,
		z0 = svcadd_u16 (z1, z0, 270),
		z0 = svcadd (z1, z0, 270))

/*
** cadd_270_u16_untied:
**	movprfx	z0, z1
**	cadd	z0\.h, z0\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_u16_untied, svuint16_t,
		z0 = svcadd_u16 (z1, z2, 270),
		z0 = svcadd (z1, z2, 270))
