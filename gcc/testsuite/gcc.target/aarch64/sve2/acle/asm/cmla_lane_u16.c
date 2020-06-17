/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_lane_0_0_u16_tied1:
**	cmla	z0\.h, z1\.h, z2\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_u16_tied1, svuint16_t,
		z0 = svcmla_lane_u16 (z0, z1, z2, 0, 0),
		z0 = svcmla_lane (z0, z1, z2, 0, 0))

/*
** cmla_lane_0_0_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, \1\.h, z2\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_u16_tied2, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z0, z2, 0, 0),
		z0 = svcmla_lane (z1, z0, z2, 0, 0))

/*
** cmla_lane_0_0_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, \1\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_u16_tied3, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z2, z0, 0, 0),
		z0 = svcmla_lane (z1, z2, z0, 0, 0))

/*
** cmla_lane_0_0_u16_untied:
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, z3\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_u16_untied, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z2, z3, 0, 0),
		z0 = svcmla_lane (z1, z2, z3, 0, 0))

/*
** cmla_lane_0_90_u16_tied1:
**	cmla	z0\.h, z1\.h, z2\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_u16_tied1, svuint16_t,
		z0 = svcmla_lane_u16 (z0, z1, z2, 0, 90),
		z0 = svcmla_lane (z0, z1, z2, 0, 90))

/*
** cmla_lane_0_90_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, \1\.h, z2\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_u16_tied2, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z0, z2, 0, 90),
		z0 = svcmla_lane (z1, z0, z2, 0, 90))

/*
** cmla_lane_0_90_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, \1\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_u16_tied3, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z2, z0, 0, 90),
		z0 = svcmla_lane (z1, z2, z0, 0, 90))

/*
** cmla_lane_0_90_u16_untied:
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, z3\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_u16_untied, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z2, z3, 0, 90),
		z0 = svcmla_lane (z1, z2, z3, 0, 90))

/*
** cmla_lane_0_180_u16_tied1:
**	cmla	z0\.h, z1\.h, z2\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_u16_tied1, svuint16_t,
		z0 = svcmla_lane_u16 (z0, z1, z2, 0, 180),
		z0 = svcmla_lane (z0, z1, z2, 0, 180))

/*
** cmla_lane_0_180_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, \1\.h, z2\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_u16_tied2, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z0, z2, 0, 180),
		z0 = svcmla_lane (z1, z0, z2, 0, 180))

/*
** cmla_lane_0_180_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, \1\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_u16_tied3, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z2, z0, 0, 180),
		z0 = svcmla_lane (z1, z2, z0, 0, 180))

/*
** cmla_lane_0_180_u16_untied:
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, z3\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_u16_untied, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z2, z3, 0, 180),
		z0 = svcmla_lane (z1, z2, z3, 0, 180))

/*
** cmla_lane_0_270_u16_tied1:
**	cmla	z0\.h, z1\.h, z2\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_u16_tied1, svuint16_t,
		z0 = svcmla_lane_u16 (z0, z1, z2, 0, 270),
		z0 = svcmla_lane (z0, z1, z2, 0, 270))

/*
** cmla_lane_0_270_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, \1\.h, z2\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_u16_tied2, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z0, z2, 0, 270),
		z0 = svcmla_lane (z1, z0, z2, 0, 270))

/*
** cmla_lane_0_270_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, \1\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_u16_tied3, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z2, z0, 0, 270),
		z0 = svcmla_lane (z1, z2, z0, 0, 270))

/*
** cmla_lane_0_270_u16_untied:
**	movprfx	z0, z1
**	cmla	z0\.h, z2\.h, z3\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_u16_untied, svuint16_t,
		z0 = svcmla_lane_u16 (z1, z2, z3, 0, 270),
		z0 = svcmla_lane (z1, z2, z3, 0, 270))

/*
** cmla_lane_1_u16:
**	cmla	z0\.h, z1\.h, z2\.h\[1\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_1_u16, svuint16_t,
		z0 = svcmla_lane_u16 (z0, z1, z2, 1, 0),
		z0 = svcmla_lane (z0, z1, z2, 1, 0))

/*
** cmla_lane_2_u16:
**	cmla	z0\.h, z1\.h, z2\.h\[2\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_2_u16, svuint16_t,
		z0 = svcmla_lane_u16 (z0, z1, z2, 2, 0),
		z0 = svcmla_lane (z0, z1, z2, 2, 0))

/*
** cmla_lane_3_u16:
**	cmla	z0\.h, z1\.h, z2\.h\[3\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_3_u16, svuint16_t,
		z0 = svcmla_lane_u16 (z0, z1, z2, 3, 0),
		z0 = svcmla_lane (z0, z1, z2, 3, 0))

/*
** cmla_lane_z8_u16:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	cmla	z0\.h, z1\.h, \1\.h\[1\], #0
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (cmla_lane_z8_u16, svuint16_t, svuint16_t, z8,
		    z0 = svcmla_lane_u16 (z0, z1, z8, 1, 0),
		    z0 = svcmla_lane (z0, z1, z8, 1, 0))

/*
** cmla_lane_z16_u16:
**	mov	(z[0-7])\.d, z16\.d
**	cmla	z0\.h, z1\.h, \1\.h\[1\], #0
**	ret
*/
TEST_DUAL_LANE_REG (cmla_lane_z16_u16, svuint16_t, svuint16_t, z16,
		    z0 = svcmla_lane_u16 (z0, z1, z16, 1, 0),
		    z0 = svcmla_lane (z0, z1, z16, 1, 0))
