/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sbclt_u32_tied1:
**	sbclt	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (sbclt_u32_tied1, svuint32_t,
		z0 = svsbclt_u32 (z0, z1, z2),
		z0 = svsbclt (z0, z1, z2))

/*
** sbclt_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sbclt	z0\.s, \1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (sbclt_u32_tied2, svuint32_t,
		z0 = svsbclt_u32 (z1, z0, z2),
		z0 = svsbclt (z1, z0, z2))

/*
** sbclt_u32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sbclt	z0\.s, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (sbclt_u32_tied3, svuint32_t,
		z0 = svsbclt_u32 (z1, z2, z0),
		z0 = svsbclt (z1, z2, z0))

/*
** sbclt_u32_untied:
**	movprfx	z0, z1
**	sbclt	z0\.s, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (sbclt_u32_untied, svuint32_t,
		z0 = svsbclt_u32 (z1, z2, z3),
		z0 = svsbclt (z1, z2, z3))

/*
** sbclt_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	sbclt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (sbclt_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svsbclt_n_u32 (z0, z1, x0),
		 z0 = svsbclt (z0, z1, x0))

/*
** sbclt_w0_u32_tied2:
**	mov	(z[0-9]+\.s), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sbclt	z0\.s, \2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (sbclt_w0_u32_tied2, svuint32_t, uint32_t,
		 z0 = svsbclt_n_u32 (z1, z0, x0),
		 z0 = svsbclt (z1, z0, x0))

/*
** sbclt_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	sbclt	z0\.s, z2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (sbclt_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svsbclt_n_u32 (z1, z2, x0),
		 z0 = svsbclt (z1, z2, x0))

/*
** sbclt_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	sbclt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sbclt_11_u32_tied1, svuint32_t,
		z0 = svsbclt_n_u32 (z0, z1, 11),
		z0 = svsbclt (z0, z1, 11))

/*
** sbclt_11_u32_tied2:
**	mov	(z[0-9]+\.s), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sbclt	z0\.s, \2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sbclt_11_u32_tied2, svuint32_t,
		z0 = svsbclt_n_u32 (z1, z0, 11),
		z0 = svsbclt (z1, z0, 11))

/*
** sbclt_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	sbclt	z0\.s, z2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sbclt_11_u32_untied, svuint32_t,
		z0 = svsbclt_n_u32 (z1, z2, 11),
		z0 = svsbclt (z1, z2, 11))
