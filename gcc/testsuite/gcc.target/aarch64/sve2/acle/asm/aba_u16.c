/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** aba_u16_tied1:
**	uaba	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (aba_u16_tied1, svuint16_t,
		z0 = svaba_u16 (z0, z1, z2),
		z0 = svaba (z0, z1, z2))

/*
** aba_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uaba	z0\.h, \1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (aba_u16_tied2, svuint16_t,
		z0 = svaba_u16 (z1, z0, z2),
		z0 = svaba (z1, z0, z2))

/*
** aba_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uaba	z0\.h, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (aba_u16_tied3, svuint16_t,
		z0 = svaba_u16 (z1, z2, z0),
		z0 = svaba (z1, z2, z0))

/*
** aba_u16_untied:
**	movprfx	z0, z1
**	uaba	z0\.h, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (aba_u16_untied, svuint16_t,
		z0 = svaba_u16 (z1, z2, z3),
		z0 = svaba (z1, z2, z3))

/*
** aba_w0_u16_tied1:
**	mov	(z[0-9]+\.h), w0
**	uaba	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_u16_tied1, svuint16_t, uint16_t,
		 z0 = svaba_n_u16 (z0, z1, x0),
		 z0 = svaba (z0, z1, x0))

/*
** aba_w0_u16_tied2:
**	mov	(z[0-9]+\.h), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uaba	z0\.h, \2\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_u16_tied2, svuint16_t, uint16_t,
		 z0 = svaba_n_u16 (z1, z0, x0),
		 z0 = svaba (z1, z0, x0))

/*
** aba_w0_u16_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	uaba	z0\.h, z2\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_u16_untied, svuint16_t, uint16_t,
		 z0 = svaba_n_u16 (z1, z2, x0),
		 z0 = svaba (z1, z2, x0))

/*
** aba_11_u16_tied1:
**	mov	(z[0-9]+\.h), #11
**	uaba	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_u16_tied1, svuint16_t,
		z0 = svaba_n_u16 (z0, z1, 11),
		z0 = svaba (z0, z1, 11))

/*
** aba_11_u16_tied2:
**	mov	(z[0-9]+\.h), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uaba	z0\.h, \2\.h, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_u16_tied2, svuint16_t,
		z0 = svaba_n_u16 (z1, z0, 11),
		z0 = svaba (z1, z0, 11))

/*
** aba_11_u16_untied:
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	uaba	z0\.h, z2\.h, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_u16_untied, svuint16_t,
		z0 = svaba_n_u16 (z1, z2, 11),
		z0 = svaba (z1, z2, 11))
