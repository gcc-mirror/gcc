/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eor3_s16_tied1:
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_s16_tied1, svint16_t,
		z0 = sveor3_s16 (z0, z1, z2),
		z0 = sveor3 (z0, z1, z2))

/*
** eor3_s16_tied2:
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_s16_tied2, svint16_t,
		z0 = sveor3_s16 (z1, z0, z2),
		z0 = sveor3 (z1, z0, z2))

/*
** eor3_s16_tied3:
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_s16_tied3, svint16_t,
		z0 = sveor3_s16 (z1, z2, z0),
		z0 = sveor3 (z1, z2, z0))

/*
** eor3_s16_untied:
** (
**	movprfx	z0, z1
**	eor3	z0\.d, z0\.d, (z2\.d, z3\.d|z3\.d, z2\.d)
** |
**	movprfx	z0, z2
**	eor3	z0\.d, z0\.d, (z1\.d, z3\.d|z3\.d, z1\.d)
** |
**	movprfx	z0, z3
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
** )
**	ret
*/
TEST_UNIFORM_Z (eor3_s16_untied, svint16_t,
		z0 = sveor3_s16 (z1, z2, z3),
		z0 = sveor3 (z1, z2, z3))

/*
** eor3_w0_s16_tied1:
**	mov	(z[0-9]+)\.h, w0
**	eor3	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (eor3_w0_s16_tied1, svint16_t, int16_t,
		 z0 = sveor3_n_s16 (z0, z1, x0),
		 z0 = sveor3 (z0, z1, x0))

/*
** eor3_w0_s16_tied2:
**	mov	(z[0-9]+)\.h, w0
**	eor3	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (eor3_w0_s16_tied2, svint16_t, int16_t,
		 z0 = sveor3_n_s16 (z1, z0, x0),
		 z0 = sveor3 (z1, z0, x0))

/*
** eor3_w0_s16_untied:
**	mov	z0\.h, w0
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (eor3_w0_s16_untied, svint16_t, int16_t,
		 z0 = sveor3_n_s16 (z1, z2, x0),
		 z0 = sveor3 (z1, z2, x0))

/*
** eor3_11_s16_tied1:
**	mov	(z[0-9]+)\.h, #11
**	eor3	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_11_s16_tied1, svint16_t,
		z0 = sveor3_n_s16 (z0, z1, 11),
		z0 = sveor3 (z0, z1, 11))

/*
** eor3_11_s16_tied2:
**	mov	(z[0-9]+)\.h, #11
**	eor3	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_11_s16_tied2, svint16_t,
		z0 = sveor3_n_s16 (z1, z0, 11),
		z0 = sveor3 (z1, z0, 11))

/*
** eor3_11_s16_untied:
**	mov	z0\.h, #11
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_11_s16_untied, svint16_t,
		z0 = sveor3_n_s16 (z1, z2, 11),
		z0 = sveor3 (z1, z2, 11))
