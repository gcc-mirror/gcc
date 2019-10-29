/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** splice_s64_tied1:
**	splice	z0\.d, p0, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (splice_s64_tied1, svint64_t,
		z0 = svsplice_s64 (p0, z0, z1),
		z0 = svsplice (p0, z0, z1))

/*
** splice_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	splice	z0\.d, p0, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (splice_s64_tied2, svint64_t,
		z0 = svsplice_s64 (p0, z1, z0),
		z0 = svsplice (p0, z1, z0))

/*
** splice_s64_untied:
**	movprfx	z0, z1
**	splice	z0\.d, p0, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (splice_s64_untied, svint64_t,
		z0 = svsplice_s64 (p0, z1, z2),
		z0 = svsplice (p0, z1, z2))
