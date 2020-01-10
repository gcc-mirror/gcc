/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eor3_s32_tied1:
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_s32_tied1, svint32_t,
		z0 = sveor3_s32 (z0, z1, z2),
		z0 = sveor3 (z0, z1, z2))

/*
** eor3_s32_tied2:
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_s32_tied2, svint32_t,
		z0 = sveor3_s32 (z1, z0, z2),
		z0 = sveor3 (z1, z0, z2))

/*
** eor3_s32_tied3:
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_s32_tied3, svint32_t,
		z0 = sveor3_s32 (z1, z2, z0),
		z0 = sveor3 (z1, z2, z0))

/*
** eor3_s32_untied:
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
TEST_UNIFORM_Z (eor3_s32_untied, svint32_t,
		z0 = sveor3_s32 (z1, z2, z3),
		z0 = sveor3 (z1, z2, z3))

/*
** eor3_w0_s32_tied1:
**	mov	(z[0-9]+)\.s, w0
**	eor3	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (eor3_w0_s32_tied1, svint32_t, int32_t,
		 z0 = sveor3_n_s32 (z0, z1, x0),
		 z0 = sveor3 (z0, z1, x0))

/*
** eor3_w0_s32_tied2:
**	mov	(z[0-9]+)\.s, w0
**	eor3	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (eor3_w0_s32_tied2, svint32_t, int32_t,
		 z0 = sveor3_n_s32 (z1, z0, x0),
		 z0 = sveor3 (z1, z0, x0))

/*
** eor3_w0_s32_untied:
**	mov	z0\.s, w0
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (eor3_w0_s32_untied, svint32_t, int32_t,
		 z0 = sveor3_n_s32 (z1, z2, x0),
		 z0 = sveor3 (z1, z2, x0))

/*
** eor3_11_s32_tied1:
**	mov	(z[0-9]+)\.s, #11
**	eor3	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_11_s32_tied1, svint32_t,
		z0 = sveor3_n_s32 (z0, z1, 11),
		z0 = sveor3 (z0, z1, 11))

/*
** eor3_11_s32_tied2:
**	mov	(z[0-9]+)\.s, #11
**	eor3	z0\.d, z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_11_s32_tied2, svint32_t,
		z0 = sveor3_n_s32 (z1, z0, 11),
		z0 = sveor3 (z1, z0, 11))

/*
** eor3_11_s32_untied:
**	mov	z0\.s, #11
**	eor3	z0\.d, z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor3_11_s32_untied, svint32_t,
		z0 = sveor3_n_s32 (z1, z2, 11),
		z0 = sveor3 (z1, z2, 11))
