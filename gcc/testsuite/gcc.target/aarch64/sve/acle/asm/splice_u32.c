/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** splice_u32_tied1:
**	splice	z0\.s, p0, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (splice_u32_tied1, svuint32_t,
		z0 = svsplice_u32 (p0, z0, z1),
		z0 = svsplice (p0, z0, z1))

/*
** splice_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	splice	z0\.s, p0, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (splice_u32_tied2, svuint32_t,
		z0 = svsplice_u32 (p0, z1, z0),
		z0 = svsplice (p0, z1, z0))

/*
** splice_u32_untied:
**	movprfx	z0, z1
**	splice	z0\.s, p0, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (splice_u32_untied, svuint32_t,
		z0 = svsplice_u32 (p0, z1, z2),
		z0 = svsplice (p0, z1, z2))
