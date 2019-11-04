/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** splice_u16_tied1:
**	splice	z0\.h, p0, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (splice_u16_tied1, svuint16_t,
		z0 = svsplice_u16 (p0, z0, z1),
		z0 = svsplice (p0, z0, z1))

/*
** splice_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	splice	z0\.h, p0, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (splice_u16_tied2, svuint16_t,
		z0 = svsplice_u16 (p0, z1, z0),
		z0 = svsplice (p0, z1, z0))

/*
** splice_u16_untied:
**	movprfx	z0, z1
**	splice	z0\.h, p0, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (splice_u16_untied, svuint16_t,
		z0 = svsplice_u16 (p0, z1, z2),
		z0 = svsplice (p0, z1, z2))
