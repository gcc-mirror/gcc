/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qinch_pat_1_s16_tied:
**	sqinch	z0\.h, pow2
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_1_s16_tied, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_POW2, 1),
		z0 = svqinch_pat (z0, SV_POW2, 1))

/*
** qinch_pat_1_s16_untied:
**	movprfx	z0, z1
**	sqinch	z0\.h, pow2
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_1_s16_untied, svint16_t,
		z0 = svqinch_pat_s16 (z1, SV_POW2, 1),
		z0 = svqinch_pat (z1, SV_POW2, 1))

/*
** qinch_pat_2_s16:
**	sqinch	z0\.h, pow2, mul #2
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_2_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_POW2, 2),
		z0 = svqinch_pat (z0, SV_POW2, 2))

/*
** qinch_pat_7_s16:
**	sqinch	z0\.h, pow2, mul #7
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_7_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_POW2, 7),
		z0 = svqinch_pat (z0, SV_POW2, 7))

/*
** qinch_pat_15_s16:
**	sqinch	z0\.h, pow2, mul #15
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_15_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_POW2, 15),
		z0 = svqinch_pat (z0, SV_POW2, 15))

/*
** qinch_pat_16_s16:
**	sqinch	z0\.h, pow2, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_16_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_POW2, 16),
		z0 = svqinch_pat (z0, SV_POW2, 16))

/*
** qinch_pat_vl1_s16:
**	sqinch	z0\.h, vl1, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl1_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL1, 16),
		z0 = svqinch_pat (z0, SV_VL1, 16))

/*
** qinch_pat_vl2_s16:
**	sqinch	z0\.h, vl2, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl2_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL2, 16),
		z0 = svqinch_pat (z0, SV_VL2, 16))

/*
** qinch_pat_vl3_s16:
**	sqinch	z0\.h, vl3, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl3_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL3, 16),
		z0 = svqinch_pat (z0, SV_VL3, 16))

/*
** qinch_pat_vl4_s16:
**	sqinch	z0\.h, vl4, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl4_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL4, 16),
		z0 = svqinch_pat (z0, SV_VL4, 16))

/*
** qinch_pat_vl5_s16:
**	sqinch	z0\.h, vl5, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl5_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL5, 16),
		z0 = svqinch_pat (z0, SV_VL5, 16))

/*
** qinch_pat_vl6_s16:
**	sqinch	z0\.h, vl6, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl6_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL6, 16),
		z0 = svqinch_pat (z0, SV_VL6, 16))

/*
** qinch_pat_vl7_s16:
**	sqinch	z0\.h, vl7, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl7_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL7, 16),
		z0 = svqinch_pat (z0, SV_VL7, 16))

/*
** qinch_pat_vl8_s16:
**	sqinch	z0\.h, vl8, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl8_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL8, 16),
		z0 = svqinch_pat (z0, SV_VL8, 16))

/*
** qinch_pat_vl16_s16:
**	sqinch	z0\.h, vl16, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl16_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL16, 16),
		z0 = svqinch_pat (z0, SV_VL16, 16))

/*
** qinch_pat_vl32_s16:
**	sqinch	z0\.h, vl32, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl32_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL32, 16),
		z0 = svqinch_pat (z0, SV_VL32, 16))

/*
** qinch_pat_vl64_s16:
**	sqinch	z0\.h, vl64, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl64_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL64, 16),
		z0 = svqinch_pat (z0, SV_VL64, 16))

/*
** qinch_pat_vl128_s16:
**	sqinch	z0\.h, vl128, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl128_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL128, 16),
		z0 = svqinch_pat (z0, SV_VL128, 16))

/*
** qinch_pat_vl256_s16:
**	sqinch	z0\.h, vl256, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_vl256_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_VL256, 16),
		z0 = svqinch_pat (z0, SV_VL256, 16))

/*
** qinch_pat_mul4_s16:
**	sqinch	z0\.h, mul4, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_mul4_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_MUL4, 16),
		z0 = svqinch_pat (z0, SV_MUL4, 16))

/*
** qinch_pat_mul3_s16:
**	sqinch	z0\.h, mul3, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_mul3_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_MUL3, 16),
		z0 = svqinch_pat (z0, SV_MUL3, 16))

/*
** qinch_pat_all_s16:
**	sqinch	z0\.h, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qinch_pat_all_s16, svint16_t,
		z0 = svqinch_pat_s16 (z0, SV_ALL, 16),
		z0 = svqinch_pat (z0, SV_ALL, 16))
