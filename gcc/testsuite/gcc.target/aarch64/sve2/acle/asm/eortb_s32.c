/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eortb_s32_tied1:
**	eortb	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (eortb_s32_tied1, svint32_t,
		z0 = sveortb_s32 (z0, z1, z2),
		z0 = sveortb (z0, z1, z2))

/*
** eortb_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.s, \1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (eortb_s32_tied2, svint32_t,
		z0 = sveortb_s32 (z1, z0, z2),
		z0 = sveortb (z1, z0, z2))

/*
** eortb_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.s, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (eortb_s32_tied3, svint32_t,
		z0 = sveortb_s32 (z1, z2, z0),
		z0 = sveortb (z1, z2, z0))

/*
** eortb_s32_untied:
**	movprfx	z0, z1
**	eortb	z0\.s, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (eortb_s32_untied, svint32_t,
		z0 = sveortb_s32 (z1, z2, z3),
		z0 = sveortb (z1, z2, z3))

/*
** eortb_w0_s32_tied1:
**	mov	(z[0-9]+\.s), w0
**	eortb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (eortb_w0_s32_tied1, svint32_t, int32_t,
		 z0 = sveortb_n_s32 (z0, z1, x0),
		 z0 = sveortb (z0, z1, x0))

/*
** eortb_w0_s32_tied2:
**	mov	(z[0-9]+\.s), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.s, \2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (eortb_w0_s32_tied2, svint32_t, int32_t,
		 z0 = sveortb_n_s32 (z1, z0, x0),
		 z0 = sveortb (z1, z0, x0))

/*
** eortb_w0_s32_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	eortb	z0\.s, z2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (eortb_w0_s32_untied, svint32_t, int32_t,
		 z0 = sveortb_n_s32 (z1, z2, x0),
		 z0 = sveortb (z1, z2, x0))

/*
** eortb_11_s32_tied1:
**	mov	(z[0-9]+\.s), #11
**	eortb	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eortb_11_s32_tied1, svint32_t,
		z0 = sveortb_n_s32 (z0, z1, 11),
		z0 = sveortb (z0, z1, 11))

/*
** eortb_11_s32_tied2:
**	mov	(z[0-9]+\.s), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eortb	z0\.s, \2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eortb_11_s32_tied2, svint32_t,
		z0 = sveortb_n_s32 (z1, z0, 11),
		z0 = sveortb (z1, z0, 11))

/*
** eortb_11_s32_untied:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	eortb	z0\.s, z2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eortb_11_s32_untied, svint32_t,
		z0 = sveortb_n_s32 (z1, z2, 11),
		z0 = sveortb (z1, z2, 11))
