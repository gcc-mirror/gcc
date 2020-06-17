/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_0_u16_tied1:
**	cmla	z0\.h, z1\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_u16_tied1, svuint16_t,
		z0 = svcmla_u16 (z0, z1, z2, 0),
		z0 = svcmla (z0, z1, z2, 0))

/*
** cmla_0_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, \1\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_u16_tied2, svuint16_t,
		z0 = svcmla_u16 (z1, z0, z2, 0),
		z0 = svcmla (z1, z0, z2, 0))

/*
** cmla_0_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, \1\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_u16_tied3, svuint16_t,
		z0 = svcmla_u16 (z1, z2, z0, 0),
		z0 = svcmla (z1, z2, z0, 0))

/*
** cmla_0_u16_untied:
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, z3\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_u16_untied, svuint16_t,
		z0 = svcmla_u16 (z1, z2, z3, 0),
		z0 = svcmla (z1, z2, z3, 0))

/*
** cmla_90_u16_tied1:
**	cmla	z0\.h, z1\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_u16_tied1, svuint16_t,
		z0 = svcmla_u16 (z0, z1, z2, 90),
		z0 = svcmla (z0, z1, z2, 90))

/*
** cmla_90_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, \1\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_u16_tied2, svuint16_t,
		z0 = svcmla_u16 (z1, z0, z2, 90),
		z0 = svcmla (z1, z0, z2, 90))

/*
** cmla_90_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, \1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_u16_tied3, svuint16_t,
		z0 = svcmla_u16 (z1, z2, z0, 90),
		z0 = svcmla (z1, z2, z0, 90))

/*
** cmla_90_u16_untied:
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, z3\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_u16_untied, svuint16_t,
		z0 = svcmla_u16 (z1, z2, z3, 90),
		z0 = svcmla (z1, z2, z3, 90))

/*
** cmla_180_u16_tied1:
**	cmla	z0\.h, z1\.h, z2\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_u16_tied1, svuint16_t,
		z0 = svcmla_u16 (z0, z1, z2, 180),
		z0 = svcmla (z0, z1, z2, 180))

/*
** cmla_180_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, \1\.h, z2\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_u16_tied2, svuint16_t,
		z0 = svcmla_u16 (z1, z0, z2, 180),
		z0 = svcmla (z1, z0, z2, 180))

/*
** cmla_180_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, \1\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_u16_tied3, svuint16_t,
		z0 = svcmla_u16 (z1, z2, z0, 180),
		z0 = svcmla (z1, z2, z0, 180))

/*
** cmla_180_u16_untied:
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, z3\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_u16_untied, svuint16_t,
		z0 = svcmla_u16 (z1, z2, z3, 180),
		z0 = svcmla (z1, z2, z3, 180))

/*
** cmla_270_u16_tied1:
**	cmla	z0\.h, z1\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_u16_tied1, svuint16_t,
		z0 = svcmla_u16 (z0, z1, z2, 270),
		z0 = svcmla (z0, z1, z2, 270))

/*
** cmla_270_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, \1\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_u16_tied2, svuint16_t,
		z0 = svcmla_u16 (z1, z0, z2, 270),
		z0 = svcmla (z1, z0, z2, 270))

/*
** cmla_270_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, \1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_u16_tied3, svuint16_t,
		z0 = svcmla_u16 (z1, z2, z0, 270),
		z0 = svcmla (z1, z2, z0, 270))

/*
** cmla_270_u16_untied:
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, z3\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_u16_untied, svuint16_t,
		z0 = svcmla_u16 (z1, z2, z3, 270),
		z0 = svcmla (z1, z2, z3, 270))
