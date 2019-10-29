/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** whilelt_rr_b8_s32:
**	whilelt	p0\.b, w0, w1
**	ret
*/
TEST_COMPARE_S (whilelt_rr_b8_s32, int32_t,
		p0 = svwhilelt_b8_s32 (x0, x1),
		p0 = svwhilelt_b8 (x0, x1))

/*
** whilelt_0r_b8_s32:
**	whilelt	p0\.b, wzr, w1
**	ret
*/
TEST_COMPARE_S (whilelt_0r_b8_s32, int32_t,
		p0 = svwhilelt_b8_s32 (0, x1),
		p0 = svwhilelt_b8 (0, x1))

/*
** whilelt_5r_b8_s32:
**	mov	(w[0-9]+), #?5
**	whilelt	p0\.b, \1, w1
**	ret
*/
TEST_COMPARE_S (whilelt_5r_b8_s32, int32_t,
		p0 = svwhilelt_b8_s32 (5, x1),
		p0 = svwhilelt_b8 (5, x1))

/*
** whilelt_r0_b8_s32:
**	whilelt	p0\.b, w0, wzr
**	ret
*/
TEST_COMPARE_S (whilelt_r0_b8_s32, int32_t,
		p0 = svwhilelt_b8_s32 (x0, 0),
		p0 = svwhilelt_b8 (x0, 0))

/*
** whilelt_r5_b8_s32:
**	mov	(w[0-9]+), #?5
**	whilelt	p0\.b, w0, \1
**	ret
*/
TEST_COMPARE_S (whilelt_r5_b8_s32, int32_t,
		p0 = svwhilelt_b8_s32 (x0, 5),
		p0 = svwhilelt_b8 (x0, 5))

/*
** whilelt_rr_b8_s64:
**	whilelt	p0\.b, x0, x1
**	ret
*/
TEST_COMPARE_S (whilelt_rr_b8_s64, int64_t,
		p0 = svwhilelt_b8_s64 (x0, x1),
		p0 = svwhilelt_b8 (x0, x1))

/*
** whilelt_0r_b8_s64:
**	whilelt	p0\.b, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilelt_0r_b8_s64, int64_t,
		p0 = svwhilelt_b8_s64 (0, x1),
		p0 = svwhilelt_b8 ((int64_t) 0, x1))

/*
** whilelt_5r_b8_s64:
**	mov	(x[0-9]+), #?5
**	whilelt	p0\.b, \1, x1
**	ret
*/
TEST_COMPARE_S (whilelt_5r_b8_s64, int64_t,
		p0 = svwhilelt_b8_s64 (5, x1),
		p0 = svwhilelt_b8 ((int64_t) 5, x1))

/*
** whilelt_r0_b8_s64:
**	whilelt	p0\.b, x0, xzr
**	ret
*/
TEST_COMPARE_S (whilelt_r0_b8_s64, int64_t,
		p0 = svwhilelt_b8_s64 (x0, 0),
		p0 = svwhilelt_b8 (x0, (int64_t) 0))

/*
** whilelt_r5_b8_s64:
**	mov	(x[0-9]+), #?5
**	whilelt	p0\.b, x0, \1
**	ret
*/
TEST_COMPARE_S (whilelt_r5_b8_s64, int64_t,
		p0 = svwhilelt_b8_s64 (x0, 5),
		p0 = svwhilelt_b8 (x0, (int64_t) 5))

/*
** whilelt_rr_b8_u32:
**	whilelo	p0\.b, w0, w1
**	ret
*/
TEST_COMPARE_S (whilelt_rr_b8_u32, uint32_t,
		p0 = svwhilelt_b8_u32 (x0, x1),
		p0 = svwhilelt_b8 (x0, x1))

/*
** whilelt_0r_b8_u32:
**	whilelo	p0\.b, wzr, w1
**	ret
*/
TEST_COMPARE_S (whilelt_0r_b8_u32, uint32_t,
		p0 = svwhilelt_b8_u32 (0, x1),
		p0 = svwhilelt_b8 ((uint32_t) 0, x1))

/*
** whilelt_5r_b8_u32:
**	mov	(w[0-9]+), #?5
**	whilelo	p0\.b, \1, w1
**	ret
*/
TEST_COMPARE_S (whilelt_5r_b8_u32, uint32_t,
		p0 = svwhilelt_b8_u32 (5, x1),
		p0 = svwhilelt_b8 ((uint32_t) 5, x1))

/*
** whilelt_r5_b8_u32:
**	mov	(w[0-9]+), #?5
**	whilelo	p0\.b, w0, \1
**	ret
*/
TEST_COMPARE_S (whilelt_r5_b8_u32, uint32_t,
		p0 = svwhilelt_b8_u32 (x0, 5),
		p0 = svwhilelt_b8 (x0, (uint32_t) 5))

/*
** whilelt_rr_b8_u64:
**	whilelo	p0\.b, x0, x1
**	ret
*/
TEST_COMPARE_S (whilelt_rr_b8_u64, uint64_t,
		p0 = svwhilelt_b8_u64 (x0, x1),
		p0 = svwhilelt_b8 (x0, x1))

/*
** whilelt_0r_b8_u64:
**	whilelo	p0\.b, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilelt_0r_b8_u64, uint64_t,
		p0 = svwhilelt_b8_u64 (0, x1),
		p0 = svwhilelt_b8 ((uint64_t) 0, x1))

/*
** whilelt_5r_b8_u64:
**	mov	(x[0-9]+), #?5
**	whilelo	p0\.b, \1, x1
**	ret
*/
TEST_COMPARE_S (whilelt_5r_b8_u64, uint64_t,
		p0 = svwhilelt_b8_u64 (5, x1),
		p0 = svwhilelt_b8 ((uint64_t) 5, x1))

/*
** whilelt_r5_b8_u64:
**	mov	(x[0-9]+), #?5
**	whilelo	p0\.b, x0, \1
**	ret
*/
TEST_COMPARE_S (whilelt_r5_b8_u64, uint64_t,
		p0 = svwhilelt_b8_u64 (x0, 5),
		p0 = svwhilelt_b8 (x0, (uint64_t) 5))
