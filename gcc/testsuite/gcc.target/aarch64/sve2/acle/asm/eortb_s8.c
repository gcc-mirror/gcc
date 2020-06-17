/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eortb_s8_tied1:
**	eortb	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (eortb_s8_tied1, svint8_t,
		z0 = sveortb_s8 (z0, z1, z2),
		z0 = sveortb (z0, z1, z2))

/*
** eortb_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.b, \1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (eortb_s8_tied2, svint8_t,
		z0 = sveortb_s8 (z1, z0, z2),
		z0 = sveortb (z1, z0, z2))

/*
** eortb_s8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.b, z2\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (eortb_s8_tied3, svint8_t,
		z0 = sveortb_s8 (z1, z2, z0),
		z0 = sveortb (z1, z2, z0))

/*
** eortb_s8_untied:
**	movprfx	z0, z1
**	eortb	z0\.b, z2\.b, z3\.b
**	ret
*/
TEST_UNIFORM_Z (eortb_s8_untied, svint8_t,
		z0 = sveortb_s8 (z1, z2, z3),
		z0 = sveortb (z1, z2, z3))

/*
** eortb_w0_s8_tied1:
**	mov	(z[0-9]+\.b), w0
**	eortb	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (eortb_w0_s8_tied1, svint8_t, int8_t,
		 z0 = sveortb_n_s8 (z0, z1, x0),
		 z0 = sveortb (z0, z1, x0))

/*
** eortb_w0_s8_tied2:
**	mov	(z[0-9]+\.b), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.b, \2\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (eortb_w0_s8_tied2, svint8_t, int8_t,
		 z0 = sveortb_n_s8 (z1, z0, x0),
		 z0 = sveortb (z1, z0, x0))

/*
** eortb_w0_s8_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	eortb	z0\.b, z2\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (eortb_w0_s8_untied, svint8_t, int8_t,
		 z0 = sveortb_n_s8 (z1, z2, x0),
		 z0 = sveortb (z1, z2, x0))

/*
** eortb_11_s8_tied1:
**	mov	(z[0-9]+\.b), #11
**	eortb	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (eortb_11_s8_tied1, svint8_t,
		z0 = sveortb_n_s8 (z0, z1, 11),
		z0 = sveortb (z0, z1, 11))

/*
** eortb_11_s8_tied2:
**	mov	(z[0-9]+\.b), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.b, \2\.b, \1
**	ret
*/
TEST_UNIFORM_Z (eortb_11_s8_tied2, svint8_t,
		z0 = sveortb_n_s8 (z1, z0, 11),
		z0 = sveortb (z1, z0, 11))

/*
** eortb_11_s8_untied:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	eortb	z0\.b, z2\.b, \1
**	ret
*/
TEST_UNIFORM_Z (eortb_11_s8_untied, svint8_t,
		z0 = sveortb_n_s8 (z1, z2, 11),
		z0 = sveortb (z1, z2, 11))
