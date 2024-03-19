/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tmad_0_f16_tied1:
**	ftmad	z0\.h, z0\.h, z1\.h, #0
**	ret
*/
TEST_UNIFORM_Z (tmad_0_f16_tied1, svfloat16_t,
		z0 = svtmad_f16 (z0, z1, 0),
		z0 = svtmad (z0, z1, 0))

/*
** tmad_0_f16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ftmad	z0\.h, z0\.h, \1\.h, #0
**	ret
*/
TEST_UNIFORM_Z (tmad_0_f16_tied2, svfloat16_t,
		z0 = svtmad_f16 (z1, z0, 0),
		z0 = svtmad (z1, z0, 0))

/*
** tmad_0_f16_untied:
**	movprfx	z0, z1
**	ftmad	z0\.h, z0\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (tmad_0_f16_untied, svfloat16_t,
		z0 = svtmad_f16 (z1, z2, 0),
		z0 = svtmad (z1, z2, 0))

/*
** tmad_1_f16:
**	ftmad	z0\.h, z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (tmad_1_f16, svfloat16_t,
		z0 = svtmad_f16 (z0, z1, 1),
		z0 = svtmad (z0, z1, 1))

/*
** tmad_2_f16:
**	ftmad	z0\.h, z0\.h, z1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (tmad_2_f16, svfloat16_t,
		z0 = svtmad_f16 (z0, z1, 2),
		z0 = svtmad (z0, z1, 2))

/*
** tmad_3_f16:
**	ftmad	z0\.h, z0\.h, z1\.h, #3
**	ret
*/
TEST_UNIFORM_Z (tmad_3_f16, svfloat16_t,
		z0 = svtmad_f16 (z0, z1, 3),
		z0 = svtmad (z0, z1, 3))

/*
** tmad_4_f16:
**	ftmad	z0\.h, z0\.h, z1\.h, #4
**	ret
*/
TEST_UNIFORM_Z (tmad_4_f16, svfloat16_t,
		z0 = svtmad_f16 (z0, z1, 4),
		z0 = svtmad (z0, z1, 4))

/*
** tmad_5_f16:
**	ftmad	z0\.h, z0\.h, z1\.h, #5
**	ret
*/
TEST_UNIFORM_Z (tmad_5_f16, svfloat16_t,
		z0 = svtmad_f16 (z0, z1, 5),
		z0 = svtmad (z0, z1, 5))

/*
** tmad_6_f16:
**	ftmad	z0\.h, z0\.h, z1\.h, #6
**	ret
*/
TEST_UNIFORM_Z (tmad_6_f16, svfloat16_t,
		z0 = svtmad_f16 (z0, z1, 6),
		z0 = svtmad (z0, z1, 6))

/*
** tmad_7_f16:
**	ftmad	z0\.h, z0\.h, z1\.h, #7
**	ret
*/
TEST_UNIFORM_Z (tmad_7_f16, svfloat16_t,
		z0 = svtmad_f16 (z0, z1, 7),
		z0 = svtmad (z0, z1, 7))
