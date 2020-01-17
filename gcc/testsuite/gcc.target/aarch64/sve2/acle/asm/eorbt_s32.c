/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eorbt_s32_tied1:
**	eorbt	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (eorbt_s32_tied1, svint32_t,
		z0 = sveorbt_s32 (z0, z1, z2),
		z0 = sveorbt (z0, z1, z2))

/*
** eorbt_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eorbt	z0\.s, \1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (eorbt_s32_tied2, svint32_t,
		z0 = sveorbt_s32 (z1, z0, z2),
		z0 = sveorbt (z1, z0, z2))

/*
** eorbt_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eorbt	z0\.s, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (eorbt_s32_tied3, svint32_t,
		z0 = sveorbt_s32 (z1, z2, z0),
		z0 = sveorbt (z1, z2, z0))

/*
** eorbt_s32_untied:
**	movprfx	z0, z1
**	eorbt	z0\.s, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (eorbt_s32_untied, svint32_t,
		z0 = sveorbt_s32 (z1, z2, z3),
		z0 = sveorbt (z1, z2, z3))

/*
** eorbt_w0_s32_tied1:
**	mov	(z[0-9]+\.s), w0
**	eorbt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (eorbt_w0_s32_tied1, svint32_t, int32_t,
		 z0 = sveorbt_n_s32 (z0, z1, x0),
		 z0 = sveorbt (z0, z1, x0))

/*
** eorbt_w0_s32_tied2:
**	mov	(z[0-9]+\.s), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eorbt	z0\.s, \2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (eorbt_w0_s32_tied2, svint32_t, int32_t,
		 z0 = sveorbt_n_s32 (z1, z0, x0),
		 z0 = sveorbt (z1, z0, x0))

/*
** eorbt_w0_s32_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	eorbt	z0\.s, z2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (eorbt_w0_s32_untied, svint32_t, int32_t,
		 z0 = sveorbt_n_s32 (z1, z2, x0),
		 z0 = sveorbt (z1, z2, x0))

/*
** eorbt_11_s32_tied1:
**	mov	(z[0-9]+\.s), #11
**	eorbt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eorbt_11_s32_tied1, svint32_t,
		z0 = sveorbt_n_s32 (z0, z1, 11),
		z0 = sveorbt (z0, z1, 11))

/*
** eorbt_11_s32_tied2:
**	mov	(z[0-9]+\.s), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eorbt	z0\.s, \2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eorbt_11_s32_tied2, svint32_t,
		z0 = sveorbt_n_s32 (z1, z0, 11),
		z0 = sveorbt (z1, z0, 11))

/*
** eorbt_11_s32_untied:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	eorbt	z0\.s, z2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eorbt_11_s32_untied, svint32_t,
		z0 = sveorbt_n_s32 (z1, z2, 11),
		z0 = sveorbt (z1, z2, 11))
