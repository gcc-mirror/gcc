/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_lane_0_0_f16_tied1:
**	fcmla	z0\.h, z1\.h, z2\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_f16_tied1, svfloat16_t,
		z0 = svcmla_lane_f16 (z0, z1, z2, 0, 0),
		z0 = svcmla_lane (z0, z1, z2, 0, 0))

/*
** cmla_lane_0_0_f16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, \1\.h, z2\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_f16_tied2, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z0, z2, 0, 0),
		z0 = svcmla_lane (z1, z0, z2, 0, 0))

/*
** cmla_lane_0_0_f16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, z2\.h, \1\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_f16_tied3, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z2, z0, 0, 0),
		z0 = svcmla_lane (z1, z2, z0, 0, 0))

/*
** cmla_lane_0_0_f16_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, z2\.h, z3\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_0_f16_untied, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z2, z3, 0, 0),
		z0 = svcmla_lane (z1, z2, z3, 0, 0))

/*
** cmla_lane_0_90_f16_tied1:
**	fcmla	z0\.h, z1\.h, z2\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_f16_tied1, svfloat16_t,
		z0 = svcmla_lane_f16 (z0, z1, z2, 0, 90),
		z0 = svcmla_lane (z0, z1, z2, 0, 90))

/*
** cmla_lane_0_90_f16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, \1\.h, z2\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_f16_tied2, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z0, z2, 0, 90),
		z0 = svcmla_lane (z1, z0, z2, 0, 90))

/*
** cmla_lane_0_90_f16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, z2\.h, \1\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_f16_tied3, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z2, z0, 0, 90),
		z0 = svcmla_lane (z1, z2, z0, 0, 90))

/*
** cmla_lane_0_90_f16_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, z2\.h, z3\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_90_f16_untied, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z2, z3, 0, 90),
		z0 = svcmla_lane (z1, z2, z3, 0, 90))

/*
** cmla_lane_0_180_f16_tied1:
**	fcmla	z0\.h, z1\.h, z2\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_f16_tied1, svfloat16_t,
		z0 = svcmla_lane_f16 (z0, z1, z2, 0, 180),
		z0 = svcmla_lane (z0, z1, z2, 0, 180))

/*
** cmla_lane_0_180_f16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, \1\.h, z2\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_f16_tied2, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z0, z2, 0, 180),
		z0 = svcmla_lane (z1, z0, z2, 0, 180))

/*
** cmla_lane_0_180_f16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, z2\.h, \1\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_f16_tied3, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z2, z0, 0, 180),
		z0 = svcmla_lane (z1, z2, z0, 0, 180))

/*
** cmla_lane_0_180_f16_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, z2\.h, z3\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_180_f16_untied, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z2, z3, 0, 180),
		z0 = svcmla_lane (z1, z2, z3, 0, 180))

/*
** cmla_lane_0_270_f16_tied1:
**	fcmla	z0\.h, z1\.h, z2\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_f16_tied1, svfloat16_t,
		z0 = svcmla_lane_f16 (z0, z1, z2, 0, 270),
		z0 = svcmla_lane (z0, z1, z2, 0, 270))

/*
** cmla_lane_0_270_f16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, \1\.h, z2\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_f16_tied2, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z0, z2, 0, 270),
		z0 = svcmla_lane (z1, z0, z2, 0, 270))

/*
** cmla_lane_0_270_f16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, z2\.h, \1\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_f16_tied3, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z2, z0, 0, 270),
		z0 = svcmla_lane (z1, z2, z0, 0, 270))

/*
** cmla_lane_0_270_f16_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, z2\.h, z3\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_0_270_f16_untied, svfloat16_t,
		z0 = svcmla_lane_f16 (z1, z2, z3, 0, 270),
		z0 = svcmla_lane (z1, z2, z3, 0, 270))

/*
** cmla_lane_1_f16:
**	fcmla	z0\.h, z1\.h, z2\.h\[1\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_1_f16, svfloat16_t,
		z0 = svcmla_lane_f16 (z0, z1, z2, 1, 0),
		z0 = svcmla_lane (z0, z1, z2, 1, 0))

/*
** cmla_lane_2_f16:
**	fcmla	z0\.h, z1\.h, z2\.h\[2\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_2_f16, svfloat16_t,
		z0 = svcmla_lane_f16 (z0, z1, z2, 2, 0),
		z0 = svcmla_lane (z0, z1, z2, 2, 0))

/*
** cmla_lane_3_f16:
**	fcmla	z0\.h, z1\.h, z2\.h\[3\], #0
**	ret
*/
TEST_UNIFORM_Z (cmla_lane_3_f16, svfloat16_t,
		z0 = svcmla_lane_f16 (z0, z1, z2, 3, 0),
		z0 = svcmla_lane (z0, z1, z2, 3, 0))
