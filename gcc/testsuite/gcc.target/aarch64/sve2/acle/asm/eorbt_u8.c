/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eorbt_u8_tied1:
**	eorbt	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (eorbt_u8_tied1, svuint8_t,
		z0 = sveorbt_u8 (z0, z1, z2),
		z0 = sveorbt (z0, z1, z2))

/*
** eorbt_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eorbt	z0\.b, \1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (eorbt_u8_tied2, svuint8_t,
		z0 = sveorbt_u8 (z1, z0, z2),
		z0 = sveorbt (z1, z0, z2))

/*
** eorbt_u8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eorbt	z0\.b, z2\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (eorbt_u8_tied3, svuint8_t,
		z0 = sveorbt_u8 (z1, z2, z0),
		z0 = sveorbt (z1, z2, z0))

/*
** eorbt_u8_untied:
**	movprfx	z0, z1
**	eorbt	z0\.b, z2\.b, z3\.b
**	ret
*/
TEST_UNIFORM_Z (eorbt_u8_untied, svuint8_t,
		z0 = sveorbt_u8 (z1, z2, z3),
		z0 = sveorbt (z1, z2, z3))

/*
** eorbt_w0_u8_tied1:
**	mov	(z[0-9]+\.b), w0
**	eorbt	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (eorbt_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = sveorbt_n_u8 (z0, z1, x0),
		 z0 = sveorbt (z0, z1, x0))

/*
** eorbt_w0_u8_tied2:
**	mov	(z[0-9]+\.b), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eorbt	z0\.b, \2\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (eorbt_w0_u8_tied2, svuint8_t, uint8_t,
		 z0 = sveorbt_n_u8 (z1, z0, x0),
		 z0 = sveorbt (z1, z0, x0))

/*
** eorbt_w0_u8_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	eorbt	z0\.b, z2\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (eorbt_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = sveorbt_n_u8 (z1, z2, x0),
		 z0 = sveorbt (z1, z2, x0))

/*
** eorbt_11_u8_tied1:
**	mov	(z[0-9]+\.b), #11
**	eorbt	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (eorbt_11_u8_tied1, svuint8_t,
		z0 = sveorbt_n_u8 (z0, z1, 11),
		z0 = sveorbt (z0, z1, 11))

/*
** eorbt_11_u8_tied2:
**	mov	(z[0-9]+\.b), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eorbt	z0\.b, \2\.b, \1
**	ret
*/
TEST_UNIFORM_Z (eorbt_11_u8_tied2, svuint8_t,
		z0 = sveorbt_n_u8 (z1, z0, 11),
		z0 = sveorbt (z1, z0, 11))

/*
** eorbt_11_u8_untied:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	eorbt	z0\.b, z2\.b, \1
**	ret
*/
TEST_UNIFORM_Z (eorbt_11_u8_untied, svuint8_t,
		z0 = sveorbt_n_u8 (z1, z2, 11),
		z0 = sveorbt (z1, z2, 11))
