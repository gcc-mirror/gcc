/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** dot_lane_0_u16_tied1:
**	udot	z0\.h, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_u16_tied1, svuint16_t, svuint8_t,
	     z0 = svdot_lane_u16_u8 (z0, z4, z5, 0),
	     z0 = svdot_lane (z0, z4, z5, 0))

/*
** dot_lane_0_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.h, \1\.b, z1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_u16_tied2, svuint16_t, svuint8_t,
		 z0_res = svdot_lane_u16_u8 (z4, z0, z1, 0),
		 z0_res = svdot_lane (z4, z0, z1, 0))

/*
** dot_lane_0_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.h, z1\.b, \1\.b\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_u16_tied3, svuint16_t, svuint8_t,
		 z0_res = svdot_lane_u16_u8 (z4, z1, z0, 0),
		 z0_res = svdot_lane (z4, z1, z0, 0))

/*
** dot_lane_0_u16_untied:
**	movprfx	z0, z1
**	udot	z0\.h, z4\.b, z5\.b\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_u16_untied, svuint16_t, svuint8_t,
	     z0 = svdot_lane_u16_u8 (z1, z4, z5, 0),
	     z0 = svdot_lane (z1, z4, z5, 0))

/*
** dot_lane_1_u16:
**	udot	z0\.h, z4\.b, z5\.b\[1\]
**	ret
*/
TEST_DUAL_Z (dot_lane_1_u16, svuint16_t, svuint8_t,
	     z0 = svdot_lane_u16_u8 (z0, z4, z5, 1),
	     z0 = svdot_lane (z0, z4, z5, 1))

/*
** dot_lane_2_u16:
**	udot	z0\.h, z4\.b, z5\.b\[2\]
**	ret
*/
TEST_DUAL_Z (dot_lane_2_u16, svuint16_t, svuint8_t,
	     z0 = svdot_lane_u16_u8 (z0, z4, z5, 2),
	     z0 = svdot_lane (z0, z4, z5, 2))

/*
** dot_lane_3_u16:
**	udot	z0\.h, z4\.b, z5\.b\[3\]
**	ret
*/
TEST_DUAL_Z (dot_lane_3_u16, svuint16_t, svuint8_t,
	     z0 = svdot_lane_u16_u8 (z0, z4, z5, 3),
	     z0 = svdot_lane (z0, z4, z5, 3))

/*
** dot_lane_z8_u16:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	udot	z0\.h, z1\.b, \1\.b\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z8_u16, svuint16_t, svuint8_t, z8,
		    z0 = svdot_lane_u16_u8 (z0, z1, z8, 1),
		    z0 = svdot_lane (z0, z1, z8, 1))

/*
** dot_lane_z16_u16:
**	mov	(z[0-7])\.d, z16\.d
**	udot	z0\.h, z1\.b, \1\.b\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z16_u16, svuint16_t, svuint8_t, z16,
		    z0 = svdot_lane_u16_u8 (z0, z1, z16, 1),
		    z0 = svdot_lane (z0, z1, z16, 1))
