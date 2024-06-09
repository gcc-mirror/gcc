/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** dot_lane_0_u32_tied1:
**	udot	z0\.s, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_u32_tied1, svuint32_t, svuint16_t,
	     z0 = svdot_lane_u32_u16 (z0, z4, z5, 0),
	     z0 = svdot_lane (z0, z4, z5, 0))

/*
** dot_lane_0_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.s, \1\.h, z1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_u32_tied2, svuint32_t, svuint16_t,
		 z0_res = svdot_lane_u32_u16 (z4, z0, z1, 0),
		 z0_res = svdot_lane (z4, z0, z1, 0))

/*
** dot_lane_0_u32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.s, z1\.h, \1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_u32_tied3, svuint32_t, svuint16_t,
		 z0_res = svdot_lane_u32_u16 (z4, z1, z0, 0),
		 z0_res = svdot_lane (z4, z1, z0, 0))

/*
** dot_lane_0_u32_untied:
**	movprfx	z0, z1
**	udot	z0\.s, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_u32_untied, svuint32_t, svuint16_t,
	     z0 = svdot_lane_u32_u16 (z1, z4, z5, 0),
	     z0 = svdot_lane (z1, z4, z5, 0))

/*
** dot_lane_1_u32:
**	udot	z0\.s, z4\.h, z5\.h\[1\]
**	ret
*/
TEST_DUAL_Z (dot_lane_1_u32, svuint32_t, svuint16_t,
	     z0 = svdot_lane_u32_u16 (z0, z4, z5, 1),
	     z0 = svdot_lane (z0, z4, z5, 1))

/*
** dot_lane_2_u32:
**	udot	z0\.s, z4\.h, z5\.h\[2\]
**	ret
*/
TEST_DUAL_Z (dot_lane_2_u32, svuint32_t, svuint16_t,
	     z0 = svdot_lane_u32_u16 (z0, z4, z5, 2),
	     z0 = svdot_lane (z0, z4, z5, 2))

/*
** dot_lane_3_u32:
**	udot	z0\.s, z4\.h, z5\.h\[3\]
**	ret
*/
TEST_DUAL_Z (dot_lane_3_u32, svuint32_t, svuint16_t,
	     z0 = svdot_lane_u32_u16 (z0, z4, z5, 3),
	     z0 = svdot_lane (z0, z4, z5, 3))

/*
** dot_lane_z8_u32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	udot	z0\.s, z1\.h, \1\.h\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z8_u32, svuint32_t, svuint16_t, z8,
		    z0 = svdot_lane_u32_u16 (z0, z1, z8, 1),
		    z0 = svdot_lane (z0, z1, z8, 1))

/*
** dot_lane_z16_u32:
**	mov	(z[0-7])\.d, z16\.d
**	udot	z0\.s, z1\.h, \1\.h\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z16_u32, svuint32_t, svuint16_t, z16,
		    z0 = svdot_lane_u32_u16 (z0, z1, z16, 1),
		    z0 = svdot_lane (z0, z1, z16, 1))
