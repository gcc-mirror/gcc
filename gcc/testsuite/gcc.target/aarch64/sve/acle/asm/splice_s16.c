/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** splice_s16_tied1:
**	splice	z0\.h, p0, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (splice_s16_tied1, svint16_t,
		z0 = svsplice_s16 (p0, z0, z1),
		z0 = svsplice (p0, z0, z1))

/*
** splice_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	splice	z0\.h, p0, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (splice_s16_tied2, svint16_t,
		z0 = svsplice_s16 (p0, z1, z0),
		z0 = svsplice (p0, z1, z0))

/*
** splice_s16_untied:
**	movprfx	z0, z1
**	splice	z0\.h, p0, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (splice_s16_untied, svint16_t,
		z0 = svsplice_s16 (p0, z1, z2),
		z0 = svsplice (p0, z1, z2))
