/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_lane_0_0_s32_tied1:
**	cmla	z0\.s, z1\.s, z2\.s\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_s32_tied1, svint32_t,
		z0 = svcmla_lane_s32 (z0, z1, z2, 0, 0),
		z0 = svcmla_lane (z0, z1, z2, 0, 0))

/*
** cmla_lane_0_0_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, \1\.s, z2\.s\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_s32_tied2, svint32_t,
		z0 = svcmla_lane_s32 (z1, z0, z2, 0, 0),
		z0 = svcmla_lane (z1, z0, z2, 0, 0))

/*
** cmla_lane_0_0_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, \1\.s\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_s32_tied3, svint32_t,
		z0 = svcmla_lane_s32 (z1, z2, z0, 0, 0),
		z0 = svcmla_lane (z1, z2, z0, 0, 0))

/*
** cmla_lane_0_0_s32_untied:
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, z3\.s\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_s32_untied, svint32_t,
		z0 = svcmla_lane_s32 (z1, z2, z3, 0, 0),
		z0 = svcmla_lane (z1, z2, z3, 0, 0))

/*
** cmla_lane_0_90_s32_tied1:
**	cmla	z0\.s, z1\.s, z2\.s\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_s32_tied1, svint32_t,
		z0 = svcmla_lane_s32 (z0, z1, z2, 0, 90),
		z0 = svcmla_lane (z0, z1, z2, 0, 90))

/*
** cmla_lane_0_90_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, \1\.s, z2\.s\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_s32_tied2, svint32_t,
		z0 = svcmla_lane_s32 (z1, z0, z2, 0, 90),
		z0 = svcmla_lane (z1, z0, z2, 0, 90))

/*
** cmla_lane_0_90_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, \1\.s\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_s32_tied3, svint32_t,
		z0 = svcmla_lane_s32 (z1, z2, z0, 0, 90),
		z0 = svcmla_lane (z1, z2, z0, 0, 90))

/*
** cmla_lane_0_90_s32_untied:
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, z3\.s\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_s32_untied, svint32_t,
		z0 = svcmla_lane_s32 (z1, z2, z3, 0, 90),
		z0 = svcmla_lane (z1, z2, z3, 0, 90))

/*
** cmla_lane_0_180_s32_tied1:
**	cmla	z0\.s, z1\.s, z2\.s\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_s32_tied1, svint32_t,
		z0 = svcmla_lane_s32 (z0, z1, z2, 0, 180),
		z0 = svcmla_lane (z0, z1, z2, 0, 180))

/*
** cmla_lane_0_180_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, \1\.s, z2\.s\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_s32_tied2, svint32_t,
		z0 = svcmla_lane_s32 (z1, z0, z2, 0, 180),
		z0 = svcmla_lane (z1, z0, z2, 0, 180))

/*
** cmla_lane_0_180_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, \1\.s\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_s32_tied3, svint32_t,
		z0 = svcmla_lane_s32 (z1, z2, z0, 0, 180),
		z0 = svcmla_lane (z1, z2, z0, 0, 180))

/*
** cmla_lane_0_180_s32_untied:
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, z3\.s\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_s32_untied, svint32_t,
		z0 = svcmla_lane_s32 (z1, z2, z3, 0, 180),
		z0 = svcmla_lane (z1, z2, z3, 0, 180))

/*
** cmla_lane_0_270_s32_tied1:
**	cmla	z0\.s, z1\.s, z2\.s\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_s32_tied1, svint32_t,
		z0 = svcmla_lane_s32 (z0, z1, z2, 0, 270),
		z0 = svcmla_lane (z0, z1, z2, 0, 270))

/*
** cmla_lane_0_270_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, \1\.s, z2\.s\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_s32_tied2, svint32_t,
		z0 = svcmla_lane_s32 (z1, z0, z2, 0, 270),
		z0 = svcmla_lane (z1, z0, z2, 0, 270))

/*
** cmla_lane_0_270_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, \1\.s\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_s32_tied3, svint32_t,
		z0 = svcmla_lane_s32 (z1, z2, z0, 0, 270),
		z0 = svcmla_lane (z1, z2, z0, 0, 270))

/*
** cmla_lane_0_270_s32_untied:
**	movprfx	z0, z1
**	cmla	z0\.s, z2\.s, z3\.s\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_s32_untied, svint32_t,
		z0 = svcmla_lane_s32 (z1, z2, z3, 0, 270),
		z0 = svcmla_lane (z1, z2, z3, 0, 270))

/*
** cmla_lane_1_s32:
**	cmla	z0\.s, z1\.s, z2\.s\[1\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_1_s32, svint32_t,
		z0 = svcmla_lane_s32 (z0, z1, z2, 1, 0),
		z0 = svcmla_lane (z0, z1, z2, 1, 0))

/*
** cmla_lane_z8_s32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	cmla	z0\.s, z1\.s, \1\.s\[1\], #0
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (cmla_lane_z8_s32, svint32_t, svint32_t, z8,
		    z0 = svcmla_lane_s32 (z0, z1, z8, 1, 0),
		    z0 = svcmla_lane (z0, z1, z8, 1, 0))

/*
** cmla_lane_z16_s32:
**	mov	(z[0-7])\.d, z16\.d
**	cmla	z0\.s, z1\.s, \1\.s\[1\], #0
**	ret
*/
TEST_DUAL_LANE_REG (cmla_lane_z16_s32, svint32_t, svint32_t, z16,
		    z0 = svcmla_lane_s32 (z0, z1, z16, 1, 0),
		    z0 = svcmla_lane (z0, z1, z16, 1, 0))
