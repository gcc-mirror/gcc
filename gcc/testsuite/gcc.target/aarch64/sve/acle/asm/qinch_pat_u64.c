/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qinch_pat_n_1_u64_tied:
**	uqinch	x0, pow2
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_1_u64_tied, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_POW2, 1),
		x0 = svqinch_pat (x0, SV_POW2, 1))

/*
** qinch_pat_n_1_u64_untied:
**	mov	x0, x1
**	uqinch	x0, pow2
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_1_u64_untied, uint64_t,
		x0 = svqinch_pat_n_u64 (x1, SV_POW2, 1),
		x0 = svqinch_pat (x1, SV_POW2, 1))

/*
** qinch_pat_n_2_u64:
**	uqinch	x0, pow2, mul #2
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_2_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_POW2, 2),
		x0 = svqinch_pat (x0, SV_POW2, 2))

/*
** qinch_pat_n_7_u64:
**	uqinch	x0, pow2, mul #7
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_7_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_POW2, 7),
		x0 = svqinch_pat (x0, SV_POW2, 7))

/*
** qinch_pat_n_15_u64:
**	uqinch	x0, pow2, mul #15
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_15_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_POW2, 15),
		x0 = svqinch_pat (x0, SV_POW2, 15))

/*
** qinch_pat_n_16_u64:
**	uqinch	x0, pow2, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_16_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_POW2, 16),
		x0 = svqinch_pat (x0, SV_POW2, 16))

/*
** qinch_pat_n_vl1_u64:
**	uqinch	x0, vl1, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl1_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL1, 16),
		x0 = svqinch_pat (x0, SV_VL1, 16))

/*
** qinch_pat_n_vl2_u64:
**	uqinch	x0, vl2, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl2_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL2, 16),
		x0 = svqinch_pat (x0, SV_VL2, 16))

/*
** qinch_pat_n_vl3_u64:
**	uqinch	x0, vl3, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl3_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL3, 16),
		x0 = svqinch_pat (x0, SV_VL3, 16))

/*
** qinch_pat_n_vl4_u64:
**	uqinch	x0, vl4, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl4_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL4, 16),
		x0 = svqinch_pat (x0, SV_VL4, 16))

/*
** qinch_pat_n_vl5_u64:
**	uqinch	x0, vl5, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl5_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL5, 16),
		x0 = svqinch_pat (x0, SV_VL5, 16))

/*
** qinch_pat_n_vl6_u64:
**	uqinch	x0, vl6, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl6_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL6, 16),
		x0 = svqinch_pat (x0, SV_VL6, 16))

/*
** qinch_pat_n_vl7_u64:
**	uqinch	x0, vl7, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl7_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL7, 16),
		x0 = svqinch_pat (x0, SV_VL7, 16))

/*
** qinch_pat_n_vl8_u64:
**	uqinch	x0, vl8, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl8_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL8, 16),
		x0 = svqinch_pat (x0, SV_VL8, 16))

/*
** qinch_pat_n_vl16_u64:
**	uqinch	x0, vl16, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl16_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL16, 16),
		x0 = svqinch_pat (x0, SV_VL16, 16))

/*
** qinch_pat_n_vl32_u64:
**	uqinch	x0, vl32, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl32_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL32, 16),
		x0 = svqinch_pat (x0, SV_VL32, 16))

/*
** qinch_pat_n_vl64_u64:
**	uqinch	x0, vl64, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl64_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL64, 16),
		x0 = svqinch_pat (x0, SV_VL64, 16))

/*
** qinch_pat_n_vl128_u64:
**	uqinch	x0, vl128, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl128_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL128, 16),
		x0 = svqinch_pat (x0, SV_VL128, 16))

/*
** qinch_pat_n_vl256_u64:
**	uqinch	x0, vl256, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_vl256_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_VL256, 16),
		x0 = svqinch_pat (x0, SV_VL256, 16))

/*
** qinch_pat_n_mul4_u64:
**	uqinch	x0, mul4, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_mul4_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_MUL4, 16),
		x0 = svqinch_pat (x0, SV_MUL4, 16))

/*
** qinch_pat_n_mul3_u64:
**	uqinch	x0, mul3, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_mul3_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_MUL3, 16),
		x0 = svqinch_pat (x0, SV_MUL3, 16))

/*
** qinch_pat_n_all_u64:
**	uqinch	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_pat_n_all_u64, uint64_t,
		x0 = svqinch_pat_n_u64 (x0, SV_ALL, 16),
		x0 = svqinch_pat (x0, SV_ALL, 16))
