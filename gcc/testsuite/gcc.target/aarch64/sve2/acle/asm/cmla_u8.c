/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_0_u8_tied1:
**	cmla	z0\.b, z1\.b, z2\.b, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_u8_tied1, svuint8_t,
		z0 = svcmla_u8 (z0, z1, z2, 0),
		z0 = svcmla (z0, z1, z2, 0))

/*
** cmla_0_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.b, \1\.b, z2\.b, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_u8_tied2, svuint8_t,
		z0 = svcmla_u8 (z1, z0, z2, 0),
		z0 = svcmla (z1, z0, z2, 0))

/*
** cmla_0_u8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.b, z2\.b, \1\.b, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_u8_tied3, svuint8_t,
		z0 = svcmla_u8 (z1, z2, z0, 0),
		z0 = svcmla (z1, z2, z0, 0))

/*
** cmla_0_u8_untied:
**	movprfx	z0, z1
**	cmla	z0\.b, z2\.b, z3\.b, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_u8_untied, svuint8_t,
		z0 = svcmla_u8 (z1, z2, z3, 0),
		z0 = svcmla (z1, z2, z3, 0))

/*
** cmla_90_u8_tied1:
**	cmla	z0\.b, z1\.b, z2\.b, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_u8_tied1, svuint8_t,
		z0 = svcmla_u8 (z0, z1, z2, 90),
		z0 = svcmla (z0, z1, z2, 90))

/*
** cmla_90_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.b, \1\.b, z2\.b, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_u8_tied2, svuint8_t,
		z0 = svcmla_u8 (z1, z0, z2, 90),
		z0 = svcmla (z1, z0, z2, 90))

/*
** cmla_90_u8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.b, z2\.b, \1\.b, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_u8_tied3, svuint8_t,
		z0 = svcmla_u8 (z1, z2, z0, 90),
		z0 = svcmla (z1, z2, z0, 90))

/*
** cmla_90_u8_untied:
**	movprfx	z0, z1
**	cmla	z0\.b, z2\.b, z3\.b, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_u8_untied, svuint8_t,
		z0 = svcmla_u8 (z1, z2, z3, 90),
		z0 = svcmla (z1, z2, z3, 90))

/*
** cmla_180_u8_tied1:
**	cmla	z0\.b, z1\.b, z2\.b, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_u8_tied1, svuint8_t,
		z0 = svcmla_u8 (z0, z1, z2, 180),
		z0 = svcmla (z0, z1, z2, 180))

/*
** cmla_180_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.b, \1\.b, z2\.b, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_u8_tied2, svuint8_t,
		z0 = svcmla_u8 (z1, z0, z2, 180),
		z0 = svcmla (z1, z0, z2, 180))

/*
** cmla_180_u8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.b, z2\.b, \1\.b, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_u8_tied3, svuint8_t,
		z0 = svcmla_u8 (z1, z2, z0, 180),
		z0 = svcmla (z1, z2, z0, 180))

/*
** cmla_180_u8_untied:
**	movprfx	z0, z1
**	cmla	z0\.b, z2\.b, z3\.b, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_u8_untied, svuint8_t,
		z0 = svcmla_u8 (z1, z2, z3, 180),
		z0 = svcmla (z1, z2, z3, 180))

/*
** cmla_270_u8_tied1:
**	cmla	z0\.b, z1\.b, z2\.b, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_u8_tied1, svuint8_t,
		z0 = svcmla_u8 (z0, z1, z2, 270),
		z0 = svcmla (z0, z1, z2, 270))

/*
** cmla_270_u8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.b, \1\.b, z2\.b, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_u8_tied2, svuint8_t,
		z0 = svcmla_u8 (z1, z0, z2, 270),
		z0 = svcmla (z1, z0, z2, 270))

/*
** cmla_270_u8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.b, z2\.b, \1\.b, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_u8_tied3, svuint8_t,
		z0 = svcmla_u8 (z1, z2, z0, 270),
		z0 = svcmla (z1, z2, z0, 270))

/*
** cmla_270_u8_untied:
**	movprfx	z0, z1
**	cmla	z0\.b, z2\.b, z3\.b, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_u8_untied, svuint8_t,
		z0 = svcmla_u8 (z1, z2, z3, 270),
		z0 = svcmla (z1, z2, z3, 270))
