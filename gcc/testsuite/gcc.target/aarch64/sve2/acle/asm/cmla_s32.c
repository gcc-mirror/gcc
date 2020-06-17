/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_0_s32_tied1:
**	cmla	z0\.s, z1\.s, z2\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_s32_tied1, svint32_t,
		z0 = svcmla_s32 (z0, z1, z2, 0),
		z0 = svcmla (z0, z1, z2, 0))

/*
** cmla_0_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, \1\.s, z2\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_s32_tied2, svint32_t,
		z0 = svcmla_s32 (z1, z0, z2, 0),
		z0 = svcmla (z1, z0, z2, 0))

/*
** cmla_0_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, \1\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_s32_tied3, svint32_t,
		z0 = svcmla_s32 (z1, z2, z0, 0),
		z0 = svcmla (z1, z2, z0, 0))

/*
** cmla_0_s32_untied:
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, z3\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_s32_untied, svint32_t,
		z0 = svcmla_s32 (z1, z2, z3, 0),
		z0 = svcmla (z1, z2, z3, 0))

/*
** cmla_90_s32_tied1:
**	cmla	z0\.s, z1\.s, z2\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_s32_tied1, svint32_t,
		z0 = svcmla_s32 (z0, z1, z2, 90),
		z0 = svcmla (z0, z1, z2, 90))

/*
** cmla_90_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, \1\.s, z2\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_s32_tied2, svint32_t,
		z0 = svcmla_s32 (z1, z0, z2, 90),
		z0 = svcmla (z1, z0, z2, 90))

/*
** cmla_90_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, \1\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_s32_tied3, svint32_t,
		z0 = svcmla_s32 (z1, z2, z0, 90),
		z0 = svcmla (z1, z2, z0, 90))

/*
** cmla_90_s32_untied:
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, z3\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_s32_untied, svint32_t,
		z0 = svcmla_s32 (z1, z2, z3, 90),
		z0 = svcmla (z1, z2, z3, 90))

/*
** cmla_180_s32_tied1:
**	cmla	z0\.s, z1\.s, z2\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_s32_tied1, svint32_t,
		z0 = svcmla_s32 (z0, z1, z2, 180),
		z0 = svcmla (z0, z1, z2, 180))

/*
** cmla_180_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, \1\.s, z2\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_s32_tied2, svint32_t,
		z0 = svcmla_s32 (z1, z0, z2, 180),
		z0 = svcmla (z1, z0, z2, 180))

/*
** cmla_180_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, \1\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_s32_tied3, svint32_t,
		z0 = svcmla_s32 (z1, z2, z0, 180),
		z0 = svcmla (z1, z2, z0, 180))

/*
** cmla_180_s32_untied:
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, z3\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_s32_untied, svint32_t,
		z0 = svcmla_s32 (z1, z2, z3, 180),
		z0 = svcmla (z1, z2, z3, 180))

/*
** cmla_270_s32_tied1:
**	cmla	z0\.s, z1\.s, z2\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_s32_tied1, svint32_t,
		z0 = svcmla_s32 (z0, z1, z2, 270),
		z0 = svcmla (z0, z1, z2, 270))

/*
** cmla_270_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, \1\.s, z2\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_s32_tied2, svint32_t,
		z0 = svcmla_s32 (z1, z0, z2, 270),
		z0 = svcmla (z1, z0, z2, 270))

/*
** cmla_270_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, \1\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_s32_tied3, svint32_t,
		z0 = svcmla_s32 (z1, z2, z0, 270),
		z0 = svcmla (z1, z2, z0, 270))

/*
** cmla_270_s32_untied:
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, z3\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_s32_untied, svint32_t,
		z0 = svcmla_s32 (z1, z2, z3, 270),
		z0 = svcmla (z1, z2, z3, 270))
