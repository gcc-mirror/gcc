/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eortb_u8_tied1:
**	eortb	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (eortb_u8_tied1, svuint8_t,
		z0 = sveortb_u8 (z0, z1, z2),
		z0 = sveortb (z0, z1, z2))

/*
** eortb_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.b, \1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (eortb_u8_tied2, svuint8_t,
		z0 = sveortb_u8 (z1, z0, z2),
		z0 = sveortb (z1, z0, z2))

/*
** eortb_u8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.b, z2\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (eortb_u8_tied3, svuint8_t,
		z0 = sveortb_u8 (z1, z2, z0),
		z0 = sveortb (z1, z2, z0))

/*
** eortb_u8_untied:
**	movprfx	z0, z1
**	eortb	z0\.b, z2\.b, z3\.b
**	ret
*/
TEST_UNIFORM_Z (eortb_u8_untied, svuint8_t,
		z0 = sveortb_u8 (z1, z2, z3),
		z0 = sveortb (z1, z2, z3))

/*
** eortb_w0_u8_tied1:
**	mov	(z[0-9]+\.b), w0
**	eortb	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (eortb_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = sveortb_n_u8 (z0, z1, x0),
		 z0 = sveortb (z0, z1, x0))

/*
** eortb_w0_u8_tied2:
**	mov	(z[0-9]+\.b), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.b, \2\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (eortb_w0_u8_tied2, svuint8_t, uint8_t,
		 z0 = sveortb_n_u8 (z1, z0, x0),
		 z0 = sveortb (z1, z0, x0))

/*
** eortb_w0_u8_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	eortb	z0\.b, z2\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (eortb_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = sveortb_n_u8 (z1, z2, x0),
		 z0 = sveortb (z1, z2, x0))

/*
** eortb_11_u8_tied1:
**	mov	(z[0-9]+\.b), #11
**	eortb	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (eortb_11_u8_tied1, svuint8_t,
		z0 = sveortb_n_u8 (z0, z1, 11),
		z0 = sveortb (z0, z1, 11))

/*
** eortb_11_u8_tied2:
**	mov	(z[0-9]+\.b), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.b, \2\.b, \1
**	ret
*/
TEST_UNIFORM_Z (eortb_11_u8_tied2, svuint8_t,
		z0 = sveortb_n_u8 (z1, z0, 11),
		z0 = sveortb (z1, z0, 11))

/*
** eortb_11_u8_untied:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	eortb	z0\.b, z2\.b, \1
**	ret
*/
TEST_UNIFORM_Z (eortb_11_u8_untied, svuint8_t,
		z0 = sveortb_n_u8 (z1, z2, 11),
		z0 = sveortb (z1, z2, 11))
