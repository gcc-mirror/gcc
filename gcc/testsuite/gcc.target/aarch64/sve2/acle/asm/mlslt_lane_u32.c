/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlslt_lane_0_u32_tied1:
**	umlslt	z0\.s, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (mlslt_lane_0_u32_tied1, svuint32_t, svuint16_t,
	     z0 = svmlslt_lane_u32 (z0, z4, z5, 0),
	     z0 = svmlslt_lane (z0, z4, z5, 0))

/*
** mlslt_lane_0_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlslt	z0\.s, \1\.h, z1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlslt_lane_0_u32_tied2, svuint32_t, svuint16_t,
		 z0_res = svmlslt_lane_u32 (z4, z0, z1, 0),
		 z0_res = svmlslt_lane (z4, z0, z1, 0))

/*
** mlslt_lane_0_u32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	umlslt	z0\.s, z1\.h, \1\.h\[0\]
**	ret
*/
TEST_DUAL_Z_REV (mlslt_lane_0_u32_tied3, svuint32_t, svuint16_t,
		 z0_res = svmlslt_lane_u32 (z4, z1, z0, 0),
		 z0_res = svmlslt_lane (z4, z1, z0, 0))

/*
** mlslt_lane_0_u32_untied:
**	movprfx	z0, z1
**	umlslt	z0\.s, z4\.h, z5\.h\[0\]
**	ret
*/
TEST_DUAL_Z (mlslt_lane_0_u32_untied, svuint32_t, svuint16_t,
	     z0 = svmlslt_lane_u32 (z1, z4, z5, 0),
	     z0 = svmlslt_lane (z1, z4, z5, 0))

/*
** mlslt_lane_1_u32:
**	umlslt	z0\.s, z4\.h, z5\.h\[1\]
**	ret
*/
TEST_DUAL_Z (mlslt_lane_1_u32, svuint32_t, svuint16_t,
	     z0 = svmlslt_lane_u32 (z0, z4, z5, 1),
	     z0 = svmlslt_lane (z0, z4, z5, 1))

/*
** mlslt_lane_z8_u32:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	umlslt	z0\.s, z1\.h, \1\.h\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mlslt_lane_z8_u32, svuint32_t, svuint16_t, z8,
		    z0 = svmlslt_lane_u32 (z0, z1, z8, 1),
		    z0 = svmlslt_lane (z0, z1, z8, 1))

/*
** mlslt_lane_z16_u32:
**	mov	(z[0-7])\.d, z16\.d
**	umlslt	z0\.s, z1\.h, \1\.h\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (mlslt_lane_z16_u32, svuint32_t, svuint16_t, z16,
		    z0 = svmlslt_lane_u32 (z0, z1, z16, 1),
		    z0 = svmlslt_lane (z0, z1, z16, 1))
