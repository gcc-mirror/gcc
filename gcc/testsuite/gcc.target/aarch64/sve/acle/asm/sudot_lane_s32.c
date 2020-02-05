/* { dg-require-effective-target aarch64_asm_i8mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+sve+i8mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sudot_lane_0_s32_tied1:
**	sudot	z0\.s, z2\.b, z4\.b\[0\]
**	ret
*/
TEST_TRIPLE_Z (sudot_lane_0_s32_tied1, svint32_t, svint8_t, svuint8_t,
	       z0 = svsudot_lane_s32 (z0, z2, z4, 0),
	       z0 = svsudot_lane (z0, z2, z4, 0))

/*
** sudot_lane_0_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z2
**	sudot	z0\.s, \1\.b, z4\.b\[0\]
**	ret
*/
TEST_TRIPLE_Z_REV2 (sudot_lane_0_s32_tied2, svint32_t, svint8_t, svuint8_t,
		    z0_res = svsudot_lane_s32 (z2, z0, z4, 0),
		    z0_res = svsudot_lane (z2, z0, z4, 0))

/*
** sudot_lane_0_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sudot	z0\.s, z2\.b, \1\.b\[0\]
**	ret
*/
TEST_TRIPLE_Z_REV (sudot_lane_0_s32_tied3, svint32_t, svint8_t, svuint8_t,
		   z0_res = svsudot_lane_s32 (z4, z2, z0, 0),
		   z0_res = svsudot_lane (z4, z2, z0, 0))

/*
** sudot_lane_0_s32_untied:
**	movprfx	z0, z1
**	sudot	z0\.s, z2\.b, z4\.b\[0\]
**	ret
*/
TEST_TRIPLE_Z (sudot_lane_0_s32_untied, svint32_t, svint8_t, svuint8_t,
	       z0 = svsudot_lane_s32 (z1, z2, z4, 0),
	       z0 = svsudot_lane (z1, z2, z4, 0))

/*
** sudot_lane_1_s32:
**	sudot	z0\.s, z2\.b, z5\.b\[1\]
**	ret
*/
TEST_TRIPLE_Z (sudot_lane_1_s32, svint32_t, svint8_t, svuint8_t,
	       z0 = svsudot_lane_s32 (z0, z2, z5, 1),
	       z0 = svsudot_lane (z0, z2, z5, 1))

/*
** sudot_lane_2_s32:
**	sudot	z0\.s, z2\.b, z5\.b\[2\]
**	ret
*/
TEST_TRIPLE_Z (sudot_lane_2_s32, svint32_t, svint8_t, svuint8_t,
	       z0 = svsudot_lane_s32 (z0, z2, z5, 2),
	       z0 = svsudot_lane (z0, z2, z5, 2))

/*
** sudot_lane_3_s32:
**	sudot	z0\.s, z2\.b, z5\.b\[3\]
**	ret
*/
TEST_TRIPLE_Z (sudot_lane_3_s32, svint32_t, svint8_t, svuint8_t,
	       z0 = svsudot_lane_s32 (z0, z2, z5, 3),
	       z0 = svsudot_lane (z0, z2, z5, 3))

/*
** sudot_lane_z8_s32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	sudot	z0\.s, z1\.b, \1\.b\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_TRIPLE_LANE_REG (sudot_lane_z8_s32, svint32_t, svint8_t, svuint8_t,
		      z8,
		      z0 = svsudot_lane_s32 (z0, z1, z8, 1),
		      z0 = svsudot_lane (z0, z1, z8, 1))

/*
** sudot_lane_z16_s32:
**	mov	(z[0-7])\.d, z16\.d
**	sudot	z0\.s, z1\.b, \1\.b\[1\]
**	ret
*/
TEST_TRIPLE_LANE_REG (sudot_lane_z16_s32, svint32_t, svint8_t, svuint8_t,
		      z16,
		      z0 = svsudot_lane_s32 (z0, z1, z16, 1),
		      z0 = svsudot_lane (z0, z1, z16, 1))
