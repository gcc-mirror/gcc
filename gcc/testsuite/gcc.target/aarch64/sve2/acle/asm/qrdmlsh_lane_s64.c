/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdmlsh_lane_0_s64_tied1:
**	sqrdmlsh	z0\.d, z1\.d, z2\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (qrdmlsh_lane_0_s64_tied1, svint64_t,
		z0 = svqrdmlsh_lane_s64 (z0, z1, z2, 0),
		z0 = svqrdmlsh_lane (z0, z1, z2, 0))

/*
** qrdmlsh_lane_0_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdmlsh	z0\.d, \1, z2\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (qrdmlsh_lane_0_s64_tied2, svint64_t,
		z0 = svqrdmlsh_lane_s64 (z1, z0, z2, 0),
		z0 = svqrdmlsh_lane (z1, z0, z2, 0))

/*
** qrdmlsh_lane_0_s64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdmlsh	z0\.d, z2\.d, \1\[0\]
**	ret
*/
TEST_UNIFORM_Z (qrdmlsh_lane_0_s64_tied3, svint64_t,
		z0 = svqrdmlsh_lane_s64 (z1, z2, z0, 0),
		z0 = svqrdmlsh_lane (z1, z2, z0, 0))

/*
** qrdmlsh_lane_0_s64_untied:
**	movprfx	z0, z1
**	sqrdmlsh	z0\.d, z2\.d, z3\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (qrdmlsh_lane_0_s64_untied, svint64_t,
		z0 = svqrdmlsh_lane_s64 (z1, z2, z3, 0),
		z0 = svqrdmlsh_lane (z1, z2, z3, 0))

/*
** qrdmlsh_lane_1_s64:
**	sqrdmlsh	z0\.d, z1\.d, z2\.d\[1\]
**	ret
*/
TEST_UNIFORM_Z (qrdmlsh_lane_1_s64, svint64_t,
		z0 = svqrdmlsh_lane_s64 (z0, z1, z2, 1),
		z0 = svqrdmlsh_lane (z0, z1, z2, 1))

/*
** qrdmlsh_lane_z15_s64:
**	str	d15, \[sp, -16\]!
**	sqrdmlsh	z0\.d, z1\.d, z15\.d\[1\]
**	ldr	d15, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (qrdmlsh_lane_z15_s64, svint64_t, svint64_t, z15,
		    z0 = svqrdmlsh_lane_s64 (z0, z1, z15, 1),
		    z0 = svqrdmlsh_lane (z0, z1, z15, 1))

/*
** qrdmlsh_lane_z16_s64:
**	mov	(z[0-7])\.d, z16\.d
**	sqrdmlsh	z0\.d, z1\.d, \1\.d\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (qrdmlsh_lane_z16_s64, svint64_t, svint64_t, z16,
		    z0 = svqrdmlsh_lane_s64 (z0, z1, z16, 1),
		    z0 = svqrdmlsh_lane (z0, z1, z16, 1))
