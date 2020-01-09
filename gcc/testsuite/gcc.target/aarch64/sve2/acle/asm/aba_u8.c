/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** aba_u8_tied1:
**	uaba	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (aba_u8_tied1, svuint8_t,
		z0 = svaba_u8 (z0, z1, z2),
		z0 = svaba (z0, z1, z2))

/*
** aba_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uaba	z0\.b, \1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (aba_u8_tied2, svuint8_t,
		z0 = svaba_u8 (z1, z0, z2),
		z0 = svaba (z1, z0, z2))

/*
** aba_u8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uaba	z0\.b, z2\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (aba_u8_tied3, svuint8_t,
		z0 = svaba_u8 (z1, z2, z0),
		z0 = svaba (z1, z2, z0))

/*
** aba_u8_untied:
**	movprfx	z0, z1
**	uaba	z0\.b, z2\.b, z3\.b
**	ret
*/
TEST_UNIFORM_Z (aba_u8_untied, svuint8_t,
		z0 = svaba_u8 (z1, z2, z3),
		z0 = svaba (z1, z2, z3))

/*
** aba_w0_u8_tied1:
**	mov	(z[0-9]+\.b), w0
**	uaba	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = svaba_n_u8 (z0, z1, x0),
		 z0 = svaba (z0, z1, x0))

/*
** aba_w0_u8_tied2:
**	mov	(z[0-9]+\.b), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uaba	z0\.b, \2\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_u8_tied2, svuint8_t, uint8_t,
		 z0 = svaba_n_u8 (z1, z0, x0),
		 z0 = svaba (z1, z0, x0))

/*
** aba_w0_u8_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	uaba	z0\.b, z2\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = svaba_n_u8 (z1, z2, x0),
		 z0 = svaba (z1, z2, x0))

/*
** aba_11_u8_tied1:
**	mov	(z[0-9]+\.b), #11
**	uaba	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_u8_tied1, svuint8_t,
		z0 = svaba_n_u8 (z0, z1, 11),
		z0 = svaba (z0, z1, 11))

/*
** aba_11_u8_tied2:
**	mov	(z[0-9]+\.b), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uaba	z0\.b, \2\.b, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_u8_tied2, svuint8_t,
		z0 = svaba_n_u8 (z1, z0, 11),
		z0 = svaba (z1, z0, 11))

/*
** aba_11_u8_untied:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	uaba	z0\.b, z2\.b, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_u8_untied, svuint8_t,
		z0 = svaba_n_u8 (z1, z2, 11),
		z0 = svaba (z1, z2, 11))
