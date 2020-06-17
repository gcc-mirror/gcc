/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bcax_s8_tied1:
**	bcax	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (bcax_s8_tied1, svint8_t,
		z0 = svbcax_s8 (z0, z1, z2),
		z0 = svbcax (z0, z1, z2))

/*
** bcax_s8_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bcax	z0\.d, z0\.d, (z2\.d, \1|\1, z2\.d)
**	ret
*/
TEST_UNIFORM_Z (bcax_s8_tied2, svint8_t,
		z0 = svbcax_s8 (z1, z0, z2),
		z0 = svbcax (z1, z0, z2))

/*
** bcax_s8_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bcax	z0\.d, z0\.d, (z2\.d, \1|\1, z2\.d)
**	ret
*/
TEST_UNIFORM_Z (bcax_s8_tied3, svint8_t,
		z0 = svbcax_s8 (z1, z2, z0),
		z0 = svbcax (z1, z2, z0))

/*
** bcax_s8_untied:
**	movprfx	z0, z1
**	bcax	z0\.d, z0\.d, (z2\.d, z3\.d|z3\.d, z2\.d)
**	ret
*/
TEST_UNIFORM_Z (bcax_s8_untied, svint8_t,
		z0 = svbcax_s8 (z1, z2, z3),
		z0 = svbcax (z1, z2, z3))

/*
** bcax_w0_s8_tied1:
**	mov	(z[0-9]+)\.b, w0
**	bcax	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (bcax_w0_s8_tied1, svint8_t, int8_t,
		 z0 = svbcax_n_s8 (z0, z1, x0),
		 z0 = svbcax (z0, z1, x0))

/*
** bcax_w0_s8_tied2:
**	mov	(z[0-9]+)\.b, w0
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bcax	z0\.d, z0\.d, (\1\.d, \2|\2, \1\.d)
**	ret
*/
TEST_UNIFORM_ZX (bcax_w0_s8_tied2, svint8_t, int8_t,
		 z0 = svbcax_n_s8 (z1, z0, x0),
		 z0 = svbcax (z1, z0, x0))

/*
** bcax_w0_s8_untied: { xfail *-*-*}
**	mov	(z[0-9]+)\.b, w0
**	movprfx	z0, z1
**	bcax	z0\.d, z0\.d, (z2\.d, \1\.d|\1\.d, z2\.d)
**	ret
*/
TEST_UNIFORM_ZX (bcax_w0_s8_untied, svint8_t, int8_t,
		 z0 = svbcax_n_s8 (z1, z2, x0),
		 z0 = svbcax (z1, z2, x0))

/*
** bcax_11_s8_tied1:
**	mov	(z[0-9]+)\.b, #11
**	bcax	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (bcax_11_s8_tied1, svint8_t,
		z0 = svbcax_n_s8 (z0, z1, 11),
		z0 = svbcax (z0, z1, 11))

/*
** bcax_11_s8_tied2:
**	mov	(z[0-9]+)\.b, #11
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bcax	z0\.d, z0\.d, (\1\.d, \2|\2, \1\.d)
**	ret
*/
TEST_UNIFORM_Z (bcax_11_s8_tied2, svint8_t,
		z0 = svbcax_n_s8 (z1, z0, 11),
		z0 = svbcax (z1, z0, 11))

/*
** bcax_11_s8_untied: { xfail *-*-*}
**	mov	(z[0-9]+)\.b, #11
**	movprfx	z0, z1
**	bcax	z0\.d, z0\.d, (z2\.d, \1\.d|\1\.d, z2\.d)
**	ret
*/
TEST_UNIFORM_Z (bcax_11_s8_untied, svint8_t,
		z0 = svbcax_n_s8 (z1, z2, 11),
		z0 = svbcax (z1, z2, 11))
