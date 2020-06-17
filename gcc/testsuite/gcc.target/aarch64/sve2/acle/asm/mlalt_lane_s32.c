/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlalt_lane_0_s32_tied1:
**	smlalt	z0\.s, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (mlalt_lane_0_s32_tied1, svint32_t, svint16_t,
	     z0 = svmlalt_lane_s32 (z0, z4, z5, 0),
	     z0 = svmlalt_lane (z0, z4, z5, 0))

/*
** mlalt_lane_0_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	smlalt	z0\.s, \1\.h, z1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlalt_lane_0_s32_tied2, svint32_t, svint16_t,
		 z0_res = svmlalt_lane_s32 (z4, z0, z1, 0),
		 z0_res = svmlalt_lane (z4, z0, z1, 0))

/*
** mlalt_lane_0_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	smlalt	z0\.s, z1\.h, \1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlalt_lane_0_s32_tied3, svint32_t, svint16_t,
		 z0_res = svmlalt_lane_s32 (z4, z1, z0, 0),
		 z0_res = svmlalt_lane (z4, z1, z0, 0))

/*
** mlalt_lane_0_s32_untied:
**	movprfx	z0, z1
**	smlalt	z0\.s, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (mlalt_lane_0_s32_untied, svint32_t, svint16_t,
	     z0 = svmlalt_lane_s32 (z1, z4, z5, 0),
	     z0 = svmlalt_lane (z1, z4, z5, 0))

/*
** mlalt_lane_1_s32:
**	smlalt	z0\.s, z4\.h, z5\.h\[1\]
**	ret
*/
TEST_DUAL_Z (mlalt_lane_1_s32, svint32_t, svint16_t,
	     z0 = svmlalt_lane_s32 (z0, z4, z5, 1),
	     z0 = svmlalt_lane (z0, z4, z5, 1))

/*
** mlalt_lane_z8_s32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	smlalt	z0\.s, z1\.h, \1\.h\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mlalt_lane_z8_s32, svint32_t, svint16_t, z8,
		    z0 = svmlalt_lane_s32 (z0, z1, z8, 1),
		    z0 = svmlalt_lane (z0, z1, z8, 1))

/*
** mlalt_lane_z16_s32:
**	mov	(z[0-7])\.d, z16\.d
**	smlalt	z0\.s, z1\.h, \1\.h\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (mlalt_lane_z16_s32, svint32_t, svint16_t, z16,
		    z0 = svmlalt_lane_s32 (z0, z1, z16, 1),
		    z0 = svmlalt_lane (z0, z1, z16, 1))
