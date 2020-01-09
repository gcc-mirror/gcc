/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** aba_s16_tied1:
**	saba	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (aba_s16_tied1, svint16_t,
		z0 = svaba_s16 (z0, z1, z2),
		z0 = svaba (z0, z1, z2))

/*
** aba_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	saba	z0\.h, \1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (aba_s16_tied2, svint16_t,
		z0 = svaba_s16 (z1, z0, z2),
		z0 = svaba (z1, z0, z2))

/*
** aba_s16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	saba	z0\.h, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (aba_s16_tied3, svint16_t,
		z0 = svaba_s16 (z1, z2, z0),
		z0 = svaba (z1, z2, z0))

/*
** aba_s16_untied:
**	movprfx	z0, z1
**	saba	z0\.h, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (aba_s16_untied, svint16_t,
		z0 = svaba_s16 (z1, z2, z3),
		z0 = svaba (z1, z2, z3))

/*
** aba_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	saba	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_s16_tied1, svint16_t, int16_t,
		 z0 = svaba_n_s16 (z0, z1, x0),
		 z0 = svaba (z0, z1, x0))

/*
** aba_w0_s16_tied2:
**	mov	(z[0-9]+\.h), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	saba	z0\.h, \2\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_s16_tied2, svint16_t, int16_t,
		 z0 = svaba_n_s16 (z1, z0, x0),
		 z0 = svaba (z1, z0, x0))

/*
** aba_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	saba	z0\.h, z2\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_s16_untied, svint16_t, int16_t,
		 z0 = svaba_n_s16 (z1, z2, x0),
		 z0 = svaba (z1, z2, x0))

/*
** aba_11_s16_tied1:
**	mov	(z[0-9]+\.h), #11
**	saba	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_s16_tied1, svint16_t,
		z0 = svaba_n_s16 (z0, z1, 11),
		z0 = svaba (z0, z1, 11))

/*
** aba_11_s16_tied2:
**	mov	(z[0-9]+\.h), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	saba	z0\.h, \2\.h, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_s16_tied2, svint16_t,
		z0 = svaba_n_s16 (z1, z0, 11),
		z0 = svaba (z1, z0, 11))

/*
** aba_11_s16_untied:
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	saba	z0\.h, z2\.h, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_s16_untied, svint16_t,
		z0 = svaba_n_s16 (z1, z2, 11),
		z0 = svaba (z1, z2, 11))
