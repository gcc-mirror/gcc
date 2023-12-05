/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tmad_0_f64_tied1:
**	ftmad	z0\.d, z0\.d, z1\.d, #0
**	ret
*/
TEST_UNIFORM_Z (tmad_0_f64_tied1, svfloat64_t,
		z0 = svtmad_f64 (z0, z1, 0),
		z0 = svtmad (z0, z1, 0))

/*
** tmad_0_f64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	ftmad	z0\.d, z0\.d, \1, #0
**	ret
*/
TEST_UNIFORM_Z (tmad_0_f64_tied2, svfloat64_t,
		z0 = svtmad_f64 (z1, z0, 0),
		z0 = svtmad (z1, z0, 0))

/*
** tmad_0_f64_untied:
**	movprfx	z0, z1
**	ftmad	z0\.d, z0\.d, z2\.d, #0
**	ret
*/
TEST_UNIFORM_Z (tmad_0_f64_untied, svfloat64_t,
		z0 = svtmad_f64 (z1, z2, 0),
		z0 = svtmad (z1, z2, 0))

/*
** tmad_1_f64:
**	ftmad	z0\.d, z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (tmad_1_f64, svfloat64_t,
		z0 = svtmad_f64 (z0, z1, 1),
		z0 = svtmad (z0, z1, 1))

/*
** tmad_2_f64:
**	ftmad	z0\.d, z0\.d, z1\.d, #2
**	ret
*/
TEST_UNIFORM_Z (tmad_2_f64, svfloat64_t,
		z0 = svtmad_f64 (z0, z1, 2),
		z0 = svtmad (z0, z1, 2))

/*
** tmad_3_f64:
**	ftmad	z0\.d, z0\.d, z1\.d, #3
**	ret
*/
TEST_UNIFORM_Z (tmad_3_f64, svfloat64_t,
		z0 = svtmad_f64 (z0, z1, 3),
		z0 = svtmad (z0, z1, 3))

/*
** tmad_4_f64:
**	ftmad	z0\.d, z0\.d, z1\.d, #4
**	ret
*/
TEST_UNIFORM_Z (tmad_4_f64, svfloat64_t,
		z0 = svtmad_f64 (z0, z1, 4),
		z0 = svtmad (z0, z1, 4))

/*
** tmad_5_f64:
**	ftmad	z0\.d, z0\.d, z1\.d, #5
**	ret
*/
TEST_UNIFORM_Z (tmad_5_f64, svfloat64_t,
		z0 = svtmad_f64 (z0, z1, 5),
		z0 = svtmad (z0, z1, 5))

/*
** tmad_6_f64:
**	ftmad	z0\.d, z0\.d, z1\.d, #6
**	ret
*/
TEST_UNIFORM_Z (tmad_6_f64, svfloat64_t,
		z0 = svtmad_f64 (z0, z1, 6),
		z0 = svtmad (z0, z1, 6))

/*
** tmad_7_f64:
**	ftmad	z0\.d, z0\.d, z1\.d, #7
**	ret
*/
TEST_UNIFORM_Z (tmad_7_f64, svfloat64_t,
		z0 = svtmad_f64 (z0, z1, 7),
		z0 = svtmad (z0, z1, 7))
