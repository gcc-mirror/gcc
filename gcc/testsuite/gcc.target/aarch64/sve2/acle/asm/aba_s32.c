/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** aba_s32_tied1:
**	saba	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (aba_s32_tied1, svint32_t,
		z0 = svaba_s32 (z0, z1, z2),
		z0 = svaba (z0, z1, z2))

/*
** aba_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	saba	z0\.s, \1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (aba_s32_tied2, svint32_t,
		z0 = svaba_s32 (z1, z0, z2),
		z0 = svaba (z1, z0, z2))

/*
** aba_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	saba	z0\.s, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (aba_s32_tied3, svint32_t,
		z0 = svaba_s32 (z1, z2, z0),
		z0 = svaba (z1, z2, z0))

/*
** aba_s32_untied:
**	movprfx	z0, z1
**	saba	z0\.s, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (aba_s32_untied, svint32_t,
		z0 = svaba_s32 (z1, z2, z3),
		z0 = svaba (z1, z2, z3))

/*
** aba_w0_s32_tied1:
**	mov	(z[0-9]+\.s), w0
**	saba	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_s32_tied1, svint32_t, int32_t,
		 z0 = svaba_n_s32 (z0, z1, x0),
		 z0 = svaba (z0, z1, x0))

/*
** aba_w0_s32_tied2:
**	mov	(z[0-9]+\.s), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	saba	z0\.s, \2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_s32_tied2, svint32_t, int32_t,
		 z0 = svaba_n_s32 (z1, z0, x0),
		 z0 = svaba (z1, z0, x0))

/*
** aba_w0_s32_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	saba	z0\.s, z2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (aba_w0_s32_untied, svint32_t, int32_t,
		 z0 = svaba_n_s32 (z1, z2, x0),
		 z0 = svaba (z1, z2, x0))

/*
** aba_11_s32_tied1:
**	mov	(z[0-9]+\.s), #11
**	saba	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_s32_tied1, svint32_t,
		z0 = svaba_n_s32 (z0, z1, 11),
		z0 = svaba (z0, z1, 11))

/*
** aba_11_s32_tied2:
**	mov	(z[0-9]+\.s), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	saba	z0\.s, \2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_s32_tied2, svint32_t,
		z0 = svaba_n_s32 (z1, z0, 11),
		z0 = svaba (z1, z0, 11))

/*
** aba_11_s32_untied:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	saba	z0\.s, z2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (aba_11_s32_untied, svint32_t,
		z0 = svaba_n_s32 (z1, z2, 11),
		z0 = svaba (z1, z2, 11))

/*
** aba_11_s32_zeroop1n:
**	ptrue	(p[0-7])\.b, all
**	mov	z0\.s, #11
**	sabd	z0\.s, \1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (aba_11_s32_zeroop1n, svint32_t,
		z0 = svaba_n_s32 (svdup_s32 (0), z1, 11),
		z0 = svaba (svdup_s32 (0), z1, 11))


/*
** aba_11_s32_zeroop1:
**	ptrue	(p[0-7])\.b, all
**	mov	z0\.s, #11
**	sabd	z0\.s, \1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (aba_11_s32_zeroop1, svint32_t,
		z0 = svaba_s32 (svdup_s32 (0), z1, svdup_s32 (11)),
		z0 = svaba (svdup_s32 (0), z1, svdup_s32 (11)))
