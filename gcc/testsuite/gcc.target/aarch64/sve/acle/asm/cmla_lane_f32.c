/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_lane_0_0_f32_tied1:
**	fcmla	z0\.s, z1\.s, z2\.s\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_f32_tied1, svfloat32_t,
		z0 = svcmla_lane_f32 (z0, z1, z2, 0, 0),
		z0 = svcmla_lane (z0, z1, z2, 0, 0))

/*
** cmla_lane_0_0_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, \1\.s, z2\.s\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_f32_tied2, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z0, z2, 0, 0),
		z0 = svcmla_lane (z1, z0, z2, 0, 0))

/*
** cmla_lane_0_0_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, z2\.s, \1\.s\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_f32_tied3, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z2, z0, 0, 0),
		z0 = svcmla_lane (z1, z2, z0, 0, 0))

/*
** cmla_lane_0_0_f32_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, z2\.s, z3\.s\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_f32_untied, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z2, z3, 0, 0),
		z0 = svcmla_lane (z1, z2, z3, 0, 0))

/*
** cmla_lane_0_90_f32_tied1:
**	fcmla	z0\.s, z1\.s, z2\.s\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_f32_tied1, svfloat32_t,
		z0 = svcmla_lane_f32 (z0, z1, z2, 0, 90),
		z0 = svcmla_lane (z0, z1, z2, 0, 90))

/*
** cmla_lane_0_90_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, \1\.s, z2\.s\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_f32_tied2, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z0, z2, 0, 90),
		z0 = svcmla_lane (z1, z0, z2, 0, 90))

/*
** cmla_lane_0_90_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, z2\.s, \1\.s\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_f32_tied3, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z2, z0, 0, 90),
		z0 = svcmla_lane (z1, z2, z0, 0, 90))

/*
** cmla_lane_0_90_f32_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, z2\.s, z3\.s\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_f32_untied, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z2, z3, 0, 90),
		z0 = svcmla_lane (z1, z2, z3, 0, 90))

/*
** cmla_lane_0_180_f32_tied1:
**	fcmla	z0\.s, z1\.s, z2\.s\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_f32_tied1, svfloat32_t,
		z0 = svcmla_lane_f32 (z0, z1, z2, 0, 180),
		z0 = svcmla_lane (z0, z1, z2, 0, 180))

/*
** cmla_lane_0_180_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, \1\.s, z2\.s\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_f32_tied2, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z0, z2, 0, 180),
		z0 = svcmla_lane (z1, z0, z2, 0, 180))

/*
** cmla_lane_0_180_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, z2\.s, \1\.s\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_f32_tied3, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z2, z0, 0, 180),
		z0 = svcmla_lane (z1, z2, z0, 0, 180))

/*
** cmla_lane_0_180_f32_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, z2\.s, z3\.s\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_f32_untied, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z2, z3, 0, 180),
		z0 = svcmla_lane (z1, z2, z3, 0, 180))

/*
** cmla_lane_0_270_f32_tied1:
**	fcmla	z0\.s, z1\.s, z2\.s\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_f32_tied1, svfloat32_t,
		z0 = svcmla_lane_f32 (z0, z1, z2, 0, 270),
		z0 = svcmla_lane (z0, z1, z2, 0, 270))

/*
** cmla_lane_0_270_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, \1\.s, z2\.s\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_f32_tied2, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z0, z2, 0, 270),
		z0 = svcmla_lane (z1, z0, z2, 0, 270))

/*
** cmla_lane_0_270_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, z2\.s, \1\.s\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_f32_tied3, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z2, z0, 0, 270),
		z0 = svcmla_lane (z1, z2, z0, 0, 270))

/*
** cmla_lane_0_270_f32_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, z2\.s, z3\.s\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_f32_untied, svfloat32_t,
		z0 = svcmla_lane_f32 (z1, z2, z3, 0, 270),
		z0 = svcmla_lane (z1, z2, z3, 0, 270))

/*
** cmla_lane_1_f32:
**	fcmla	z0\.s, z1\.s, z2\.s\[1\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_1_f32, svfloat32_t,
		z0 = svcmla_lane_f32 (z0, z1, z2, 1, 0),
		z0 = svcmla_lane (z0, z1, z2, 1, 0))
