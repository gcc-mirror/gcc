/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dot_lane_0_s32_tied1:
**	sdot	z0\.s, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_s32_tied1, svint32_t, svint8_t,
	     z0 = svdot_lane_s32 (z0, z4, z5, 0),
	     z0 = svdot_lane (z0, z4, z5, 0))

/*
** dot_lane_0_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sdot	z0\.s, \1\.b, z1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_s32_tied2, svint32_t, svint8_t,
		 z0_res = svdot_lane_s32 (z4, z0, z1, 0),
		 z0_res = svdot_lane (z4, z0, z1, 0))

/*
** dot_lane_0_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sdot	z0\.s, z1\.b, \1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_s32_tied3, svint32_t, svint8_t,
		 z0_res = svdot_lane_s32 (z4, z1, z0, 0),
		 z0_res = svdot_lane (z4, z1, z0, 0))

/*
** dot_lane_0_s32_untied:
**	movprfx	z0, z1
**	sdot	z0\.s, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_s32_untied, svint32_t, svint8_t,
	     z0 = svdot_lane_s32 (z1, z4, z5, 0),
	     z0 = svdot_lane (z1, z4, z5, 0))

/*
** dot_lane_1_s32:
**	sdot	z0\.s, z4\.b, z5\.b\[1\]
**	ret
*/
TEST_DUAL_Z (dot_lane_1_s32, svint32_t, svint8_t,
	     z0 = svdot_lane_s32 (z0, z4, z5, 1),
	     z0 = svdot_lane (z0, z4, z5, 1))

/*
** dot_lane_2_s32:
**	sdot	z0\.s, z4\.b, z5\.b\[2\]
**	ret
*/
TEST_DUAL_Z (dot_lane_2_s32, svint32_t, svint8_t,
	     z0 = svdot_lane_s32 (z0, z4, z5, 2),
	     z0 = svdot_lane (z0, z4, z5, 2))

/*
** dot_lane_3_s32:
**	sdot	z0\.s, z4\.b, z5\.b\[3\]
**	ret
*/
TEST_DUAL_Z (dot_lane_3_s32, svint32_t, svint8_t,
	     z0 = svdot_lane_s32 (z0, z4, z5, 3),
	     z0 = svdot_lane (z0, z4, z5, 3))

/*
** dot_lane_z8_s32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	sdot	z0\.s, z1\.b, \1\.b\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z8_s32, svint32_t, svint8_t, z8,
		    z0 = svdot_lane_s32 (z0, z1, z8, 1),
		    z0 = svdot_lane (z0, z1, z8, 1))

/*
** dot_lane_z16_s32:
**	mov	(z[0-7])\.d, z16\.d
**	sdot	z0\.s, z1\.b, \1\.b\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z16_s32, svint32_t, svint8_t, z16,
		    z0 = svdot_lane_s32 (z0, z1, z16, 1),
		    z0 = svdot_lane (z0, z1, z16, 1))
