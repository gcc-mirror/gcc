/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdcmlah_lane_0_0_s16_tied1:
**	sqrdcmlah	z0\.h, z1\.h, z2\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_0_s16_tied1, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z0, z1, z2, 0, 0),
		z0 = svqrdcmlah_lane (z0, z1, z2, 0, 0))

/*
** qrdcmlah_lane_0_0_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, \1\.h, z2\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_0_s16_tied2, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z0, z2, 0, 0),
		z0 = svqrdcmlah_lane (z1, z0, z2, 0, 0))

/*
** qrdcmlah_lane_0_0_s16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, z2\.h, \1\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_0_s16_tied3, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z2, z0, 0, 0),
		z0 = svqrdcmlah_lane (z1, z2, z0, 0, 0))

/*
** qrdcmlah_lane_0_0_s16_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, z2\.h, z3\.h\[0\], #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_0_s16_untied, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z2, z3, 0, 0),
		z0 = svqrdcmlah_lane (z1, z2, z3, 0, 0))

/*
** qrdcmlah_lane_0_90_s16_tied1:
**	sqrdcmlah	z0\.h, z1\.h, z2\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_90_s16_tied1, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z0, z1, z2, 0, 90),
		z0 = svqrdcmlah_lane (z0, z1, z2, 0, 90))

/*
** qrdcmlah_lane_0_90_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, \1\.h, z2\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_90_s16_tied2, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z0, z2, 0, 90),
		z0 = svqrdcmlah_lane (z1, z0, z2, 0, 90))

/*
** qrdcmlah_lane_0_90_s16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, z2\.h, \1\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_90_s16_tied3, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z2, z0, 0, 90),
		z0 = svqrdcmlah_lane (z1, z2, z0, 0, 90))

/*
** qrdcmlah_lane_0_90_s16_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, z2\.h, z3\.h\[0\], #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_90_s16_untied, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z2, z3, 0, 90),
		z0 = svqrdcmlah_lane (z1, z2, z3, 0, 90))

/*
** qrdcmlah_lane_0_180_s16_tied1:
**	sqrdcmlah	z0\.h, z1\.h, z2\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_180_s16_tied1, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z0, z1, z2, 0, 180),
		z0 = svqrdcmlah_lane (z0, z1, z2, 0, 180))

/*
** qrdcmlah_lane_0_180_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, \1\.h, z2\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_180_s16_tied2, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z0, z2, 0, 180),
		z0 = svqrdcmlah_lane (z1, z0, z2, 0, 180))

/*
** qrdcmlah_lane_0_180_s16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, z2\.h, \1\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_180_s16_tied3, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z2, z0, 0, 180),
		z0 = svqrdcmlah_lane (z1, z2, z0, 0, 180))

/*
** qrdcmlah_lane_0_180_s16_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, z2\.h, z3\.h\[0\], #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_180_s16_untied, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z2, z3, 0, 180),
		z0 = svqrdcmlah_lane (z1, z2, z3, 0, 180))

/*
** qrdcmlah_lane_0_270_s16_tied1:
**	sqrdcmlah	z0\.h, z1\.h, z2\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_270_s16_tied1, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z0, z1, z2, 0, 270),
		z0 = svqrdcmlah_lane (z0, z1, z2, 0, 270))

/*
** qrdcmlah_lane_0_270_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, \1\.h, z2\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_270_s16_tied2, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z0, z2, 0, 270),
		z0 = svqrdcmlah_lane (z1, z0, z2, 0, 270))

/*
** qrdcmlah_lane_0_270_s16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, z2\.h, \1\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_270_s16_tied3, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z2, z0, 0, 270),
		z0 = svqrdcmlah_lane (z1, z2, z0, 0, 270))

/*
** qrdcmlah_lane_0_270_s16_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.h, z2\.h, z3\.h\[0\], #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_0_270_s16_untied, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z1, z2, z3, 0, 270),
		z0 = svqrdcmlah_lane (z1, z2, z3, 0, 270))

/*
** qrdcmlah_lane_1_s16:
**	sqrdcmlah	z0\.h, z1\.h, z2\.h\[1\], #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_1_s16, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z0, z1, z2, 1, 0),
		z0 = svqrdcmlah_lane (z0, z1, z2, 1, 0))

/*
** qrdcmlah_lane_2_s16:
**	sqrdcmlah	z0\.h, z1\.h, z2\.h\[2\], #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_2_s16, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z0, z1, z2, 2, 0),
		z0 = svqrdcmlah_lane (z0, z1, z2, 2, 0))

/*
** qrdcmlah_lane_3_s16:
**	sqrdcmlah	z0\.h, z1\.h, z2\.h\[3\], #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_lane_3_s16, svint16_t,
		z0 = svqrdcmlah_lane_s16 (z0, z1, z2, 3, 0),
		z0 = svqrdcmlah_lane (z0, z1, z2, 3, 0))

/*
** qrdcmlah_lane_z8_s16:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	sqrdcmlah	z0\.h, z1\.h, \1\.h\[1\], #0
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (qrdcmlah_lane_z8_s16, svint16_t, svint16_t, z8,
		    z0 = svqrdcmlah_lane_s16 (z0, z1, z8, 1, 0),
		    z0 = svqrdcmlah_lane (z0, z1, z8, 1, 0))

/*
** qrdcmlah_lane_z16_s16:
**	mov	(z[0-7])\.d, z16\.d
**	sqrdcmlah	z0\.h, z1\.h, \1\.h\[1\], #0
**	ret
*/
TEST_DUAL_LANE_REG (qrdcmlah_lane_z16_s16, svint16_t, svint16_t, z16,
		    z0 = svqrdcmlah_lane_s16 (z0, z1, z16, 1, 0),
		    z0 = svqrdcmlah_lane (z0, z1, z16, 1, 0))
