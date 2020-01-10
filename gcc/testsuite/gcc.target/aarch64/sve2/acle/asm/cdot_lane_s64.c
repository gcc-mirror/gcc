/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cdot_lane_0_0_s64_tied1:
**	cdot	z0\.d, z4\.h, z5\.h\[0\], #0
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_0_s64_tied1, svint64_t, svint16_t,
	     z0 = svcdot_lane_s64 (z0, z4, z5, 0, 0),
	     z0 = svcdot_lane (z0, z4, z5, 0, 0))

/*
** cdot_lane_0_0_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, \1\.h, z1\.h\[0\], #0
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_0_s64_tied2, svint64_t, svint16_t,
		 z0_res = svcdot_lane_s64 (z4, z0, z1, 0, 0),
		 z0_res = svcdot_lane (z4, z0, z1, 0, 0))

/*
** cdot_lane_0_0_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, z1\.h, \1\.h\[0\], #0
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_0_s64_tied3, svint64_t, svint16_t,
		 z0_res = svcdot_lane_s64 (z4, z1, z0, 0, 0),
		 z0_res = svcdot_lane (z4, z1, z0, 0, 0))

/*
** cdot_lane_0_0_s64_untied:
**	movprfx	z0, z1
**	cdot	z0\.d, z4\.h, z5\.h\[0\], #0
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_0_s64_untied, svint64_t, svint16_t,
	     z0 = svcdot_lane_s64 (z1, z4, z5, 0, 0),
	     z0 = svcdot_lane (z1, z4, z5, 0, 0))

/*
** cdot_lane_0_90_s64_tied1:
**	cdot	z0\.d, z4\.h, z5\.h\[0\], #90
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_90_s64_tied1, svint64_t, svint16_t,
	     z0 = svcdot_lane_s64 (z0, z4, z5, 0, 90),
	     z0 = svcdot_lane (z0, z4, z5, 0, 90))

/*
** cdot_lane_0_90_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, \1\.h, z1\.h\[0\], #90
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_90_s64_tied2, svint64_t, svint16_t,
		 z0_res = svcdot_lane_s64 (z4, z0, z1, 0, 90),
		 z0_res = svcdot_lane (z4, z0, z1, 0, 90))

/*
** cdot_lane_0_90_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, z1\.h, \1\.h\[0\], #90
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_90_s64_tied3, svint64_t, svint16_t,
		 z0_res = svcdot_lane_s64 (z4, z1, z0, 0, 90),
		 z0_res = svcdot_lane (z4, z1, z0, 0, 90))

/*
** cdot_lane_0_90_s64_untied:
**	movprfx	z0, z1
**	cdot	z0\.d, z4\.h, z5\.h\[0\], #90
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_90_s64_untied, svint64_t, svint16_t,
	     z0 = svcdot_lane_s64 (z1, z4, z5, 0, 90),
	     z0 = svcdot_lane (z1, z4, z5, 0, 90))

/*
** cdot_lane_0_180_s64_tied1:
**	cdot	z0\.d, z4\.h, z5\.h\[0\], #180
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_180_s64_tied1, svint64_t, svint16_t,
	     z0 = svcdot_lane_s64 (z0, z4, z5, 0, 180),
	     z0 = svcdot_lane (z0, z4, z5, 0, 180))

/*
** cdot_lane_0_180_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, \1\.h, z1\.h\[0\], #180
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_180_s64_tied2, svint64_t, svint16_t,
		 z0_res = svcdot_lane_s64 (z4, z0, z1, 0, 180),
		 z0_res = svcdot_lane (z4, z0, z1, 0, 180))

/*
** cdot_lane_0_180_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, z1\.h, \1\.h\[0\], #180
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_180_s64_tied3, svint64_t, svint16_t,
		 z0_res = svcdot_lane_s64 (z4, z1, z0, 0, 180),
		 z0_res = svcdot_lane (z4, z1, z0, 0, 180))

/*
** cdot_lane_0_180_s64_untied:
**	movprfx	z0, z1
**	cdot	z0\.d, z4\.h, z5\.h\[0\], #180
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_180_s64_untied, svint64_t, svint16_t,
	     z0 = svcdot_lane_s64 (z1, z4, z5, 0, 180),
	     z0 = svcdot_lane (z1, z4, z5, 0, 180))

/*
** cdot_lane_0_270_s64_tied1:
**	cdot	z0\.d, z4\.h, z5\.h\[0\], #270
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_270_s64_tied1, svint64_t, svint16_t,
	     z0 = svcdot_lane_s64 (z0, z4, z5, 0, 270),
	     z0 = svcdot_lane (z0, z4, z5, 0, 270))

/*
** cdot_lane_0_270_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, \1\.h, z1\.h\[0\], #270
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_270_s64_tied2, svint64_t, svint16_t,
		 z0_res = svcdot_lane_s64 (z4, z0, z1, 0, 270),
		 z0_res = svcdot_lane (z4, z0, z1, 0, 270))

/*
** cdot_lane_0_270_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, z1\.h, \1\.h\[0\], #270
**	ret
*/
TEST_DUAL_Z_REV (cdot_lane_0_270_s64_tied3, svint64_t, svint16_t,
		 z0_res = svcdot_lane_s64 (z4, z1, z0, 0, 270),
		 z0_res = svcdot_lane (z4, z1, z0, 0, 270))

/*
** cdot_lane_0_270_s64_untied:
**	movprfx	z0, z1
**	cdot	z0\.d, z4\.h, z5\.h\[0\], #270
**	ret
*/
TEST_DUAL_Z (cdot_lane_0_270_s64_untied, svint64_t, svint16_t,
	     z0 = svcdot_lane_s64 (z1, z4, z5, 0, 270),
	     z0 = svcdot_lane (z1, z4, z5, 0, 270))

/*
** cdot_lane_z15_s64:
**	str	d15, \[sp, -16\]!
**	cdot	z0\.d, z1\.h, z15\.h\[1\], #0
**	ldr	d15, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (cdot_lane_z15_s64, svint64_t, svint16_t, z15,
		    z0 = svcdot_lane_s64 (z0, z1, z15, 1, 0),
		    z0 = svcdot_lane (z0, z1, z15, 1, 0))

/*
** cdot_lane_z16_s64:
**	mov	(z[0-9]|z1[0-5])\.d, z16\.d
**	cdot	z0\.d, z1\.h, \1\.h\[1\], #0
**	ret
*/
TEST_DUAL_LANE_REG (cdot_lane_z16_s64, svint64_t, svint16_t, z16,
		    z0 = svcdot_lane_s64 (z0, z1, z16, 1, 0),
		    z0 = svcdot_lane (z0, z1, z16, 1, 0))
