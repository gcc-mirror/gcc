/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dot_lane_0_u64_tied1:
**	udot	z0\.d, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_u64_tied1, svuint64_t, svuint16_t,
	     z0 = svdot_lane_u64 (z0, z4, z5, 0),
	     z0 = svdot_lane (z0, z4, z5, 0))

/*
** dot_lane_0_u64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.d, \1\.h, z1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_u64_tied2, svuint64_t, svuint16_t,
		 z0_res = svdot_lane_u64 (z4, z0, z1, 0),
		 z0_res = svdot_lane (z4, z0, z1, 0))

/*
** dot_lane_0_u64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.d, z1\.h, \1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (dot_lane_0_u64_tied3, svuint64_t, svuint16_t,
		 z0_res = svdot_lane_u64 (z4, z1, z0, 0),
		 z0_res = svdot_lane (z4, z1, z0, 0))

/*
** dot_lane_0_u64_untied:
**	movprfx	z0, z1
**	udot	z0\.d, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (dot_lane_0_u64_untied, svuint64_t, svuint16_t,
	     z0 = svdot_lane_u64 (z1, z4, z5, 0),
	     z0 = svdot_lane (z1, z4, z5, 0))

/*
** dot_lane_1_u64:
**	udot	z0\.d, z4\.h, z5\.h\[1\]
**	ret
*/
TEST_DUAL_Z (dot_lane_1_u64, svuint64_t, svuint16_t,
	     z0 = svdot_lane_u64 (z0, z4, z5, 1),
	     z0 = svdot_lane (z0, z4, z5, 1))

/*
** dot_lane_z15_u64:
**	str	d15, \[sp, -16\]!
**	udot	z0\.d, z1\.h, z15\.h\[1\]
**	ldr	d15, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z15_u64, svuint64_t, svuint16_t, z15,
		    z0 = svdot_lane_u64 (z0, z1, z15, 1),
		    z0 = svdot_lane (z0, z1, z15, 1))

/*
** dot_lane_z16_u64:
**	mov	(z[0-9]|z1[0-5])\.d, z16\.d
**	udot	z0\.d, z1\.h, \1\.h\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (dot_lane_z16_u64, svuint64_t, svuint16_t, z16,
		    z0 = svdot_lane_u64 (z0, z1, z16, 1),
		    z0 = svdot_lane (z0, z1, z16, 1))
