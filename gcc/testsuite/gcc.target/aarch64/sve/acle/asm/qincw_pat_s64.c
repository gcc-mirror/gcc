/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincw_pat_n_1_s64_tied:
**	sqincw	x0, pow2
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_1_s64_tied, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_POW2, 1),
		x0 = svqincw_pat (x0, SV_POW2, 1))

/*
** qincw_pat_n_1_s64_untied:
**	mov	x0, x1
**	sqincw	x0, pow2
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_1_s64_untied, int64_t,
		x0 = svqincw_pat_n_s64 (x1, SV_POW2, 1),
		x0 = svqincw_pat (x1, SV_POW2, 1))

/*
** qincw_pat_n_2_s64:
**	sqincw	x0, pow2, mul #2
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_2_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_POW2, 2),
		x0 = svqincw_pat (x0, SV_POW2, 2))

/*
** qincw_pat_n_7_s64:
**	sqincw	x0, pow2, mul #7
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_7_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_POW2, 7),
		x0 = svqincw_pat (x0, SV_POW2, 7))

/*
** qincw_pat_n_15_s64:
**	sqincw	x0, pow2, mul #15
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_15_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_POW2, 15),
		x0 = svqincw_pat (x0, SV_POW2, 15))

/*
** qincw_pat_n_16_s64:
**	sqincw	x0, pow2, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_16_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_POW2, 16),
		x0 = svqincw_pat (x0, SV_POW2, 16))

/*
** qincw_pat_n_vl1_s64:
**	sqincw	x0, vl1, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl1_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL1, 16),
		x0 = svqincw_pat (x0, SV_VL1, 16))

/*
** qincw_pat_n_vl2_s64:
**	sqincw	x0, vl2, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl2_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL2, 16),
		x0 = svqincw_pat (x0, SV_VL2, 16))

/*
** qincw_pat_n_vl3_s64:
**	sqincw	x0, vl3, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl3_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL3, 16),
		x0 = svqincw_pat (x0, SV_VL3, 16))

/*
** qincw_pat_n_vl4_s64:
**	sqincw	x0, vl4, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl4_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL4, 16),
		x0 = svqincw_pat (x0, SV_VL4, 16))

/*
** qincw_pat_n_vl5_s64:
**	sqincw	x0, vl5, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl5_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL5, 16),
		x0 = svqincw_pat (x0, SV_VL5, 16))

/*
** qincw_pat_n_vl6_s64:
**	sqincw	x0, vl6, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl6_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL6, 16),
		x0 = svqincw_pat (x0, SV_VL6, 16))

/*
** qincw_pat_n_vl7_s64:
**	sqincw	x0, vl7, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl7_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL7, 16),
		x0 = svqincw_pat (x0, SV_VL7, 16))

/*
** qincw_pat_n_vl8_s64:
**	sqincw	x0, vl8, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl8_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL8, 16),
		x0 = svqincw_pat (x0, SV_VL8, 16))

/*
** qincw_pat_n_vl16_s64:
**	sqincw	x0, vl16, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl16_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL16, 16),
		x0 = svqincw_pat (x0, SV_VL16, 16))

/*
** qincw_pat_n_vl32_s64:
**	sqincw	x0, vl32, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl32_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL32, 16),
		x0 = svqincw_pat (x0, SV_VL32, 16))

/*
** qincw_pat_n_vl64_s64:
**	sqincw	x0, vl64, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl64_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL64, 16),
		x0 = svqincw_pat (x0, SV_VL64, 16))

/*
** qincw_pat_n_vl128_s64:
**	sqincw	x0, vl128, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl128_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL128, 16),
		x0 = svqincw_pat (x0, SV_VL128, 16))

/*
** qincw_pat_n_vl256_s64:
**	sqincw	x0, vl256, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_vl256_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_VL256, 16),
		x0 = svqincw_pat (x0, SV_VL256, 16))

/*
** qincw_pat_n_mul4_s64:
**	sqincw	x0, mul4, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_mul4_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_MUL4, 16),
		x0 = svqincw_pat (x0, SV_MUL4, 16))

/*
** qincw_pat_n_mul3_s64:
**	sqincw	x0, mul3, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_mul3_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_MUL3, 16),
		x0 = svqincw_pat (x0, SV_MUL3, 16))

/*
** qincw_pat_n_all_s64:
**	sqincw	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_pat_n_all_s64, int64_t,
		x0 = svqincw_pat_n_s64 (x0, SV_ALL, 16),
		x0 = svqincw_pat (x0, SV_ALL, 16))
