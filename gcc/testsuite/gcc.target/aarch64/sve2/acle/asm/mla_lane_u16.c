/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mla_lane_0_u16_tied1:
**	mla	z0\.h, z1\.h, z2\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_u16_tied1, svuint16_t,
		z0 = svmla_lane_u16 (z0, z1, z2, 0),
		z0 = svmla_lane (z0, z1, z2, 0))

/*
** mla_lane_0_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mla	z0\.h, \1\.h, z2\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_u16_tied2, svuint16_t,
		z0 = svmla_lane_u16 (z1, z0, z2, 0),
		z0 = svmla_lane (z1, z0, z2, 0))

/*
** mla_lane_0_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mla	z0\.h, z2\.h, \1\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_u16_tied3, svuint16_t,
		z0 = svmla_lane_u16 (z1, z2, z0, 0),
		z0 = svmla_lane (z1, z2, z0, 0))

/*
** mla_lane_0_u16_untied:
**	movprfx	z0, z1
**	mla	z0\.h, z2\.h, z3\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_u16_untied, svuint16_t,
		z0 = svmla_lane_u16 (z1, z2, z3, 0),
		z0 = svmla_lane (z1, z2, z3, 0))

/*
** mla_lane_1_u16:
**	mla	z0\.h, z1\.h, z2\.h\[1\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_1_u16, svuint16_t,
		z0 = svmla_lane_u16 (z0, z1, z2, 1),
		z0 = svmla_lane (z0, z1, z2, 1))

/*
** mla_lane_2_u16:
**	mla	z0\.h, z1\.h, z2\.h\[2\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_2_u16, svuint16_t,
		z0 = svmla_lane_u16 (z0, z1, z2, 2),
		z0 = svmla_lane (z0, z1, z2, 2))

/*
** mla_lane_3_u16:
**	mla	z0\.h, z1\.h, z2\.h\[3\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_3_u16, svuint16_t,
		z0 = svmla_lane_u16 (z0, z1, z2, 3),
		z0 = svmla_lane (z0, z1, z2, 3))

/*
** mla_lane_4_u16:
**	mla	z0\.h, z1\.h, z2\.h\[4\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_4_u16, svuint16_t,
		z0 = svmla_lane_u16 (z0, z1, z2, 4),
		z0 = svmla_lane (z0, z1, z2, 4))

/*
** mla_lane_5_u16:
**	mla	z0\.h, z1\.h, z2\.h\[5\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_5_u16, svuint16_t,
		z0 = svmla_lane_u16 (z0, z1, z2, 5),
		z0 = svmla_lane (z0, z1, z2, 5))

/*
** mla_lane_6_u16:
**	mla	z0\.h, z1\.h, z2\.h\[6\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_6_u16, svuint16_t,
		z0 = svmla_lane_u16 (z0, z1, z2, 6),
		z0 = svmla_lane (z0, z1, z2, 6))

/*
** mla_lane_7_u16:
**	mla	z0\.h, z1\.h, z2\.h\[7\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_7_u16, svuint16_t,
		z0 = svmla_lane_u16 (z0, z1, z2, 7),
		z0 = svmla_lane (z0, z1, z2, 7))

/*
** mla_lane_z8_u16:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	mla	z0\.h, z1\.h, \1\.h\[1\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mla_lane_z8_u16, svuint16_t, svuint16_t, z8,
		    z0 = svmla_lane_u16 (z0, z1, z8, 1),
		    z0 = svmla_lane (z0, z1, z8, 1))

/*
** mla_lane_z16_u16:
**	mov	(z[0-7])\.d, z16\.d
**	mla	z0\.h, z1\.h, \1\.h\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (mla_lane_z16_u16, svuint16_t, svuint16_t, z16,
		    z0 = svmla_lane_u16 (z0, z1, z16, 1),
		    z0 = svmla_lane (z0, z1, z16, 1))
