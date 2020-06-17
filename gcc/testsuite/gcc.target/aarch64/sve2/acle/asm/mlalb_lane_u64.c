/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlalb_lane_0_u64_tied1:
**	umlalb	z0\.d, z4\.s, z5\.s\[0\]
**	ret
*/
TEST_DUAL_Z (mlalb_lane_0_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svmlalb_lane_u64 (z0, z4, z5, 0),
	     z0 = svmlalb_lane (z0, z4, z5, 0))

/*
** mlalb_lane_0_u64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlalb	z0\.d, \1\.s, z1\.s\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlalb_lane_0_u64_tied2, svuint64_t, svuint32_t,
		 z0_res = svmlalb_lane_u64 (z4, z0, z1, 0),
		 z0_res = svmlalb_lane (z4, z0, z1, 0))

/*
** mlalb_lane_0_u64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlalb	z0\.d, z1\.s, \1\.s\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlalb_lane_0_u64_tied3, svuint64_t, svuint32_t,
		 z0_res = svmlalb_lane_u64 (z4, z1, z0, 0),
		 z0_res = svmlalb_lane (z4, z1, z0, 0))

/*
** mlalb_lane_0_u64_untied:
**	movprfx	z0, z1
**	umlalb	z0\.d, z4\.s, z5\.s\[0\]
**	ret
*/
TEST_DUAL_Z (mlalb_lane_0_u64_untied, svuint64_t, svuint32_t,
	     z0 = svmlalb_lane_u64 (z1, z4, z5, 0),
	     z0 = svmlalb_lane (z1, z4, z5, 0))

/*
** mlalb_lane_z15_u64:
**	str	d15, \[sp, -16\]!
**	umlalb	z0\.d, z1\.s, z15\.s\[1\]
**	ldr	d15, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mlalb_lane_z15_u64, svuint64_t, svuint32_t, z15,
		    z0 = svmlalb_lane_u64 (z0, z1, z15, 1),
		    z0 = svmlalb_lane (z0, z1, z15, 1))

/*
** mlalb_lane_z16_u64:
**	mov	(z[0-9]|z1[0-5])\.d, z16\.d
**	umlalb	z0\.d, z1\.s, \1\.s\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (mlalb_lane_z16_u64, svuint64_t, svuint32_t, z16,
		    z0 = svmlalb_lane_u64 (z0, z1, z16, 1),
		    z0 = svmlalb_lane (z0, z1, z16, 1))
