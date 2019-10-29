/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdech_pat_n_1_s32_tied:
**	sqdech	x0, w0, pow2
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_1_s32_tied, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_POW2, 1),
		x0 = svqdech_pat (x0, SV_POW2, 1))

/*
** qdech_pat_n_1_s32_untied:
**	mov	w0, w1
**	sqdech	x0, w0, pow2
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_1_s32_untied, int32_t,
		x0 = svqdech_pat_n_s32 (x1, SV_POW2, 1),
		x0 = svqdech_pat (x1, SV_POW2, 1))

/*
** qdech_pat_n_2_s32:
**	sqdech	x0, w0, pow2, mul #2
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_2_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_POW2, 2),
		x0 = svqdech_pat (x0, SV_POW2, 2))

/*
** qdech_pat_n_7_s32:
**	sqdech	x0, w0, pow2, mul #7
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_7_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_POW2, 7),
		x0 = svqdech_pat (x0, SV_POW2, 7))

/*
** qdech_pat_n_15_s32:
**	sqdech	x0, w0, pow2, mul #15
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_15_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_POW2, 15),
		x0 = svqdech_pat (x0, SV_POW2, 15))

/*
** qdech_pat_n_16_s32:
**	sqdech	x0, w0, pow2, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_16_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_POW2, 16),
		x0 = svqdech_pat (x0, SV_POW2, 16))

/*
** qdech_pat_n_vl1_s32:
**	sqdech	x0, w0, vl1, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl1_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL1, 16),
		x0 = svqdech_pat (x0, SV_VL1, 16))

/*
** qdech_pat_n_vl2_s32:
**	sqdech	x0, w0, vl2, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl2_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL2, 16),
		x0 = svqdech_pat (x0, SV_VL2, 16))

/*
** qdech_pat_n_vl3_s32:
**	sqdech	x0, w0, vl3, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl3_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL3, 16),
		x0 = svqdech_pat (x0, SV_VL3, 16))

/*
** qdech_pat_n_vl4_s32:
**	sqdech	x0, w0, vl4, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl4_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL4, 16),
		x0 = svqdech_pat (x0, SV_VL4, 16))

/*
** qdech_pat_n_vl5_s32:
**	sqdech	x0, w0, vl5, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl5_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL5, 16),
		x0 = svqdech_pat (x0, SV_VL5, 16))

/*
** qdech_pat_n_vl6_s32:
**	sqdech	x0, w0, vl6, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl6_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL6, 16),
		x0 = svqdech_pat (x0, SV_VL6, 16))

/*
** qdech_pat_n_vl7_s32:
**	sqdech	x0, w0, vl7, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl7_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL7, 16),
		x0 = svqdech_pat (x0, SV_VL7, 16))

/*
** qdech_pat_n_vl8_s32:
**	sqdech	x0, w0, vl8, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl8_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL8, 16),
		x0 = svqdech_pat (x0, SV_VL8, 16))

/*
** qdech_pat_n_vl16_s32:
**	sqdech	x0, w0, vl16, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl16_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL16, 16),
		x0 = svqdech_pat (x0, SV_VL16, 16))

/*
** qdech_pat_n_vl32_s32:
**	sqdech	x0, w0, vl32, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl32_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL32, 16),
		x0 = svqdech_pat (x0, SV_VL32, 16))

/*
** qdech_pat_n_vl64_s32:
**	sqdech	x0, w0, vl64, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl64_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL64, 16),
		x0 = svqdech_pat (x0, SV_VL64, 16))

/*
** qdech_pat_n_vl128_s32:
**	sqdech	x0, w0, vl128, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl128_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL128, 16),
		x0 = svqdech_pat (x0, SV_VL128, 16))

/*
** qdech_pat_n_vl256_s32:
**	sqdech	x0, w0, vl256, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_vl256_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_VL256, 16),
		x0 = svqdech_pat (x0, SV_VL256, 16))

/*
** qdech_pat_n_mul4_s32:
**	sqdech	x0, w0, mul4, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_mul4_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_MUL4, 16),
		x0 = svqdech_pat (x0, SV_MUL4, 16))

/*
** qdech_pat_n_mul3_s32:
**	sqdech	x0, w0, mul3, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_mul3_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_MUL3, 16),
		x0 = svqdech_pat (x0, SV_MUL3, 16))

/*
** qdech_pat_n_all_s32:
**	sqdech	x0, w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_pat_n_all_s32, int32_t,
		x0 = svqdech_pat_n_s32 (x0, SV_ALL, 16),
		x0 = svqdech_pat (x0, SV_ALL, 16))
