/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cdot_lane_0_0_s32_tied1:
**	cdot	z0\.s, z4\.b, z5\.b\[0\], #0
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_0_s32_tied1, svint32_t, svint8_t,
	     z0 = svcdot_lane_s32 (z0, z4, z5, 0, 0),
	     z0 = svcdot_lane (z0, z4, z5, 0, 0))

/*
** cdot_lane_0_0_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, \1\.b, z1\.b\[0\], #0
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_0_s32_tied2, svint32_t, svint8_t,
		 z0_res = svcdot_lane_s32 (z4, z0, z1, 0, 0),
		 z0_res = svcdot_lane (z4, z0, z1, 0, 0))

/*
** cdot_lane_0_0_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, z1\.b, \1\.b\[0\], #0
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_0_s32_tied3, svint32_t, svint8_t,
		 z0_res = svcdot_lane_s32 (z4, z1, z0, 0, 0),
		 z0_res = svcdot_lane (z4, z1, z0, 0, 0))

/*
** cdot_lane_0_0_s32_untied:
**	movprfx	z0, z1
**	cdot	z0\.s, z4\.b, z5\.b\[0\], #0
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_0_s32_untied, svint32_t, svint8_t,
	     z0 = svcdot_lane_s32 (z1, z4, z5, 0, 0),
	     z0 = svcdot_lane (z1, z4, z5, 0, 0))

/*
** cdot_lane_0_90_s32_tied1:
**	cdot	z0\.s, z4\.b, z5\.b\[0\], #90
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_90_s32_tied1, svint32_t, svint8_t,
	     z0 = svcdot_lane_s32 (z0, z4, z5, 0, 90),
	     z0 = svcdot_lane (z0, z4, z5, 0, 90))

/*
** cdot_lane_0_90_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, \1\.b, z1\.b\[0\], #90
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_90_s32_tied2, svint32_t, svint8_t,
		 z0_res = svcdot_lane_s32 (z4, z0, z1, 0, 90),
		 z0_res = svcdot_lane (z4, z0, z1, 0, 90))

/*
** cdot_lane_0_90_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, z1\.b, \1\.b\[0\], #90
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_90_s32_tied3, svint32_t, svint8_t,
		 z0_res = svcdot_lane_s32 (z4, z1, z0, 0, 90),
		 z0_res = svcdot_lane (z4, z1, z0, 0, 90))

/*
** cdot_lane_0_90_s32_untied:
**	movprfx	z0, z1
**	cdot	z0\.s, z4\.b, z5\.b\[0\], #90
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_90_s32_untied, svint32_t, svint8_t,
	     z0 = svcdot_lane_s32 (z1, z4, z5, 0, 90),
	     z0 = svcdot_lane (z1, z4, z5, 0, 90))

/*
** cdot_lane_0_180_s32_tied1:
**	cdot	z0\.s, z4\.b, z5\.b\[0\], #180
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_180_s32_tied1, svint32_t, svint8_t,
	     z0 = svcdot_lane_s32 (z0, z4, z5, 0, 180),
	     z0 = svcdot_lane (z0, z4, z5, 0, 180))

/*
** cdot_lane_0_180_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, \1\.b, z1\.b\[0\], #180
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_180_s32_tied2, svint32_t, svint8_t,
		 z0_res = svcdot_lane_s32 (z4, z0, z1, 0, 180),
		 z0_res = svcdot_lane (z4, z0, z1, 0, 180))

/*
** cdot_lane_0_180_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, z1\.b, \1\.b\[0\], #180
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_180_s32_tied3, svint32_t, svint8_t,
		 z0_res = svcdot_lane_s32 (z4, z1, z0, 0, 180),
		 z0_res = svcdot_lane (z4, z1, z0, 0, 180))

/*
** cdot_lane_0_180_s32_untied:
**	movprfx	z0, z1
**	cdot	z0\.s, z4\.b, z5\.b\[0\], #180
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_180_s32_untied, svint32_t, svint8_t,
	     z0 = svcdot_lane_s32 (z1, z4, z5, 0, 180),
	     z0 = svcdot_lane (z1, z4, z5, 0, 180))

/*
** cdot_lane_0_270_s32_tied1:
**	cdot	z0\.s, z4\.b, z5\.b\[0\], #270
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_270_s32_tied1, svint32_t, svint8_t,
	     z0 = svcdot_lane_s32 (z0, z4, z5, 0, 270),
	     z0 = svcdot_lane (z0, z4, z5, 0, 270))

/*
** cdot_lane_0_270_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, \1\.b, z1\.b\[0\], #270
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_270_s32_tied2, svint32_t, svint8_t,
		 z0_res = svcdot_lane_s32 (z4, z0, z1, 0, 270),
		 z0_res = svcdot_lane (z4, z0, z1, 0, 270))

/*
** cdot_lane_0_270_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, z1\.b, \1\.b\[0\], #270
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_270_s32_tied3, svint32_t, svint8_t,
		 z0_res = svcdot_lane_s32 (z4, z1, z0, 0, 270),
		 z0_res = svcdot_lane (z4, z1, z0, 0, 270))

/*
** cdot_lane_0_270_s32_untied:
**	movprfx	z0, z1
**	cdot	z0\.s, z4\.b, z5\.b\[0\], #270
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_270_s32_untied, svint32_t, svint8_t,
	     z0 = svcdot_lane_s32 (z1, z4, z5, 0, 270),
	     z0 = svcdot_lane (z1, z4, z5, 0, 270))

/*
** cdot_lane_1_s32:
**	cdot	z0\.s, z4\.b, z5\.b\[1\], #0
**	ret
*/
TEST_DUAL_Z (cdot_lane_1_s32, svint32_t, svint8_t,
	     z0 = svcdot_lane_s32 (z0, z4, z5, 1, 0),
	     z0 = svcdot_lane (z0, z4, z5, 1, 0))

/*
** cdot_lane_z8_s32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	cdot	z0\.s, z1\.b, \1\.b\[1\], #0
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (cdot_lane_z8_s32, svint32_t, svint8_t, z8,
		    z0 = svcdot_lane_s32 (z0, z1, z8, 1, 0),
		    z0 = svcdot_lane (z0, z1, z8, 1, 0))

/*
** cdot_lane_z16_s32:
**	mov	(z[0-7])\.d, z16\.d
**	cdot	z0\.s, z1\.b, \1\.b\[1\], #0
**	ret
*/
TEST_DUAL_LANE_REG (cdot_lane_z16_s32, svint32_t, svint8_t, z16,
		    z0 = svcdot_lane_s32 (z0, z1, z16, 1, 0),
		    z0 = svcdot_lane (z0, z1, z16, 1, 0))
