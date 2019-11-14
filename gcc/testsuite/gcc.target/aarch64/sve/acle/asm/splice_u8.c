/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** splice_u8_tied1:
**	splice	z0\.b, p0, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (splice_u8_tied1, svuint8_t,
		z0 = svsplice_u8 (p0, z0, z1),
		z0 = svsplice (p0, z0, z1))

/*
** splice_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	splice	z0\.b, p0, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (splice_u8_tied2, svuint8_t,
		z0 = svsplice_u8 (p0, z1, z0),
		z0 = svsplice (p0, z1, z0))

/*
** splice_u8_untied:
**	movprfx	z0, z1
**	splice	z0\.b, p0, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (splice_u8_untied, svuint8_t,
		z0 = svsplice_u8 (p0, z1, z2),
		z0 = svsplice (p0, z1, z2))
