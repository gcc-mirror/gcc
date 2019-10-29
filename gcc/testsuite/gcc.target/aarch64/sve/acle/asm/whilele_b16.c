/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** whilele_rr_b16_s32:
**	whilele	p0\.h, w0, w1
**	ret
*/
TEST_COMPARE_S (whilele_rr_b16_s32, int32_t,
		p0 = svwhilele_b16_s32 (x0, x1),
		p0 = svwhilele_b16 (x0, x1))

/*
** whilele_0r_b16_s32:
**	whilele	p0\.h, wzr, w1
**	ret
*/
TEST_COMPARE_S (whilele_0r_b16_s32, int32_t,
		p0 = svwhilele_b16_s32 (0, x1),
		p0 = svwhilele_b16 (0, x1))

/*
** whilele_5r_b16_s32:
**	mov	(w[0-9]+), #?5
**	whilele	p0\.h, \1, w1
**	ret
*/
TEST_COMPARE_S (whilele_5r_b16_s32, int32_t,
		p0 = svwhilele_b16_s32 (5, x1),
		p0 = svwhilele_b16 (5, x1))

/*
** whilele_r0_b16_s32:
**	whilele	p0\.h, w0, wzr
**	ret
*/
TEST_COMPARE_S (whilele_r0_b16_s32, int32_t,
		p0 = svwhilele_b16_s32 (x0, 0),
		p0 = svwhilele_b16 (x0, 0))

/*
** whilele_r5_b16_s32:
**	mov	(w[0-9]+), #?5
**	whilele	p0\.h, w0, \1
**	ret
*/
TEST_COMPARE_S (whilele_r5_b16_s32, int32_t,
		p0 = svwhilele_b16_s32 (x0, 5),
		p0 = svwhilele_b16 (x0, 5))

/*
** whilele_rr_b16_s64:
**	whilele	p0\.h, x0, x1
**	ret
*/
TEST_COMPARE_S (whilele_rr_b16_s64, int64_t,
		p0 = svwhilele_b16_s64 (x0, x1),
		p0 = svwhilele_b16 (x0, x1))

/*
** whilele_0r_b16_s64:
**	whilele	p0\.h, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilele_0r_b16_s64, int64_t,
		p0 = svwhilele_b16_s64 (0, x1),
		p0 = svwhilele_b16 ((int64_t) 0, x1))

/*
** whilele_5r_b16_s64:
**	mov	(x[0-9]+), #?5
**	whilele	p0\.h, \1, x1
**	ret
*/
TEST_COMPARE_S (whilele_5r_b16_s64, int64_t,
		p0 = svwhilele_b16_s64 (5, x1),
		p0 = svwhilele_b16 ((int64_t) 5, x1))

/*
** whilele_r0_b16_s64:
**	whilele	p0\.h, x0, xzr
**	ret
*/
TEST_COMPARE_S (whilele_r0_b16_s64, int64_t,
		p0 = svwhilele_b16_s64 (x0, 0),
		p0 = svwhilele_b16 (x0, (int64_t) 0))

/*
** whilele_r5_b16_s64:
**	mov	(x[0-9]+), #?5
**	whilele	p0\.h, x0, \1
**	ret
*/
TEST_COMPARE_S (whilele_r5_b16_s64, int64_t,
		p0 = svwhilele_b16_s64 (x0, 5),
		p0 = svwhilele_b16 (x0, (int64_t) 5))

/*
** whilele_rr_b16_u32:
**	whilels	p0\.h, w0, w1
**	ret
*/
TEST_COMPARE_S (whilele_rr_b16_u32, uint32_t,
		p0 = svwhilele_b16_u32 (x0, x1),
		p0 = svwhilele_b16 (x0, x1))

/*
** whilele_0r_b16_u32:
**	whilels	p0\.h, wzr, w1
**	ret
*/
TEST_COMPARE_S (whilele_0r_b16_u32, uint32_t,
		p0 = svwhilele_b16_u32 (0, x1),
		p0 = svwhilele_b16 ((uint32_t) 0, x1))

/*
** whilele_5r_b16_u32:
**	mov	(w[0-9]+), #?5
**	whilels	p0\.h, \1, w1
**	ret
*/
TEST_COMPARE_S (whilele_5r_b16_u32, uint32_t,
		p0 = svwhilele_b16_u32 (5, x1),
		p0 = svwhilele_b16 ((uint32_t) 5, x1))

/*
** whilele_r5_b16_u32:
**	mov	(w[0-9]+), #?5
**	whilels	p0\.h, w0, \1
**	ret
*/
TEST_COMPARE_S (whilele_r5_b16_u32, uint32_t,
		p0 = svwhilele_b16_u32 (x0, 5),
		p0 = svwhilele_b16 (x0, (uint32_t) 5))

/*
** whilele_rr_b16_u64:
**	whilels	p0\.h, x0, x1
**	ret
*/
TEST_COMPARE_S (whilele_rr_b16_u64, uint64_t,
		p0 = svwhilele_b16_u64 (x0, x1),
		p0 = svwhilele_b16 (x0, x1))

/*
** whilele_0r_b16_u64:
**	whilels	p0\.h, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilele_0r_b16_u64, uint64_t,
		p0 = svwhilele_b16_u64 (0, x1),
		p0 = svwhilele_b16 ((uint64_t) 0, x1))

/*
** whilele_5r_b16_u64:
**	mov	(x[0-9]+), #?5
**	whilels	p0\.h, \1, x1
**	ret
*/
TEST_COMPARE_S (whilele_5r_b16_u64, uint64_t,
		p0 = svwhilele_b16_u64 (5, x1),
		p0 = svwhilele_b16 ((uint64_t) 5, x1))

/*
** whilele_r5_b16_u64:
**	mov	(x[0-9]+), #?5
**	whilels	p0\.h, x0, \1
**	ret
*/
TEST_COMPARE_S (whilele_r5_b16_u64, uint64_t,
		p0 = svwhilele_b16_u64 (x0, 5),
		p0 = svwhilele_b16 (x0, (uint64_t) 5))
