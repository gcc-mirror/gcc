/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** whilelt_rr_b64_s32:
**	whilelt	p0\.d, w0, w1
**	ret
*/
TEST_COMPARE_S (whilelt_rr_b64_s32, int32_t,
		p0 = svwhilelt_b64_s32 (x0, x1),
		p0 = svwhilelt_b64 (x0, x1))

/*
** whilelt_0r_b64_s32:
**	whilelt	p0\.d, wzr, w1
**	ret
*/
TEST_COMPARE_S (whilelt_0r_b64_s32, int32_t,
		p0 = svwhilelt_b64_s32 (0, x1),
		p0 = svwhilelt_b64 (0, x1))

/*
** whilelt_5r_b64_s32:
**	mov	(w[0-9]+), #?5
**	whilelt	p0\.d, \1, w1
**	ret
*/
TEST_COMPARE_S (whilelt_5r_b64_s32, int32_t,
		p0 = svwhilelt_b64_s32 (5, x1),
		p0 = svwhilelt_b64 (5, x1))

/*
** whilelt_r0_b64_s32:
**	whilelt	p0\.d, w0, wzr
**	ret
*/
TEST_COMPARE_S (whilelt_r0_b64_s32, int32_t,
		p0 = svwhilelt_b64_s32 (x0, 0),
		p0 = svwhilelt_b64 (x0, 0))

/*
** whilelt_r5_b64_s32:
**	mov	(w[0-9]+), #?5
**	whilelt	p0\.d, w0, \1
**	ret
*/
TEST_COMPARE_S (whilelt_r5_b64_s32, int32_t,
		p0 = svwhilelt_b64_s32 (x0, 5),
		p0 = svwhilelt_b64 (x0, 5))

/*
** whilelt_rr_b64_s64:
**	whilelt	p0\.d, x0, x1
**	ret
*/
TEST_COMPARE_S (whilelt_rr_b64_s64, int64_t,
		p0 = svwhilelt_b64_s64 (x0, x1),
		p0 = svwhilelt_b64 (x0, x1))

/*
** whilelt_0r_b64_s64:
**	whilelt	p0\.d, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilelt_0r_b64_s64, int64_t,
		p0 = svwhilelt_b64_s64 (0, x1),
		p0 = svwhilelt_b64 ((int64_t) 0, x1))

/*
** whilelt_5r_b64_s64:
**	mov	(x[0-9]+), #?5
**	whilelt	p0\.d, \1, x1
**	ret
*/
TEST_COMPARE_S (whilelt_5r_b64_s64, int64_t,
		p0 = svwhilelt_b64_s64 (5, x1),
		p0 = svwhilelt_b64 ((int64_t) 5, x1))

/*
** whilelt_r0_b64_s64:
**	whilelt	p0\.d, x0, xzr
**	ret
*/
TEST_COMPARE_S (whilelt_r0_b64_s64, int64_t,
		p0 = svwhilelt_b64_s64 (x0, 0),
		p0 = svwhilelt_b64 (x0, (int64_t) 0))

/*
** whilelt_r5_b64_s64:
**	mov	(x[0-9]+), #?5
**	whilelt	p0\.d, x0, \1
**	ret
*/
TEST_COMPARE_S (whilelt_r5_b64_s64, int64_t,
		p0 = svwhilelt_b64_s64 (x0, 5),
		p0 = svwhilelt_b64 (x0, (int64_t) 5))

/*
** whilelt_rr_b64_u32:
**	whilelo	p0\.d, w0, w1
**	ret
*/
TEST_COMPARE_S (whilelt_rr_b64_u32, uint32_t,
		p0 = svwhilelt_b64_u32 (x0, x1),
		p0 = svwhilelt_b64 (x0, x1))

/*
** whilelt_0r_b64_u32:
**	whilelo	p0\.d, wzr, w1
**	ret
*/
TEST_COMPARE_S (whilelt_0r_b64_u32, uint32_t,
		p0 = svwhilelt_b64_u32 (0, x1),
		p0 = svwhilelt_b64 ((uint32_t) 0, x1))

/*
** whilelt_5r_b64_u32:
**	mov	(w[0-9]+), #?5
**	whilelo	p0\.d, \1, w1
**	ret
*/
TEST_COMPARE_S (whilelt_5r_b64_u32, uint32_t,
		p0 = svwhilelt_b64_u32 (5, x1),
		p0 = svwhilelt_b64 ((uint32_t) 5, x1))

/*
** whilelt_r5_b64_u32:
**	mov	(w[0-9]+), #?5
**	whilelo	p0\.d, w0, \1
**	ret
*/
TEST_COMPARE_S (whilelt_r5_b64_u32, uint32_t,
		p0 = svwhilelt_b64_u32 (x0, 5),
		p0 = svwhilelt_b64 (x0, (uint32_t) 5))

/*
** whilelt_rr_b64_u64:
**	whilelo	p0\.d, x0, x1
**	ret
*/
TEST_COMPARE_S (whilelt_rr_b64_u64, uint64_t,
		p0 = svwhilelt_b64_u64 (x0, x1),
		p0 = svwhilelt_b64 (x0, x1))

/*
** whilelt_0r_b64_u64:
**	whilelo	p0\.d, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilelt_0r_b64_u64, uint64_t,
		p0 = svwhilelt_b64_u64 (0, x1),
		p0 = svwhilelt_b64 ((uint64_t) 0, x1))

/*
** whilelt_5r_b64_u64:
**	mov	(x[0-9]+), #?5
**	whilelo	p0\.d, \1, x1
**	ret
*/
TEST_COMPARE_S (whilelt_5r_b64_u64, uint64_t,
		p0 = svwhilelt_b64_u64 (5, x1),
		p0 = svwhilelt_b64 ((uint64_t) 5, x1))

/*
** whilelt_r5_b64_u64:
**	mov	(x[0-9]+), #?5
**	whilelo	p0\.d, x0, \1
**	ret
*/
TEST_COMPARE_S (whilelt_r5_b64_u64, uint64_t,
		p0 = svwhilelt_b64_u64 (x0, 5),
		p0 = svwhilelt_b64 (x0, (uint64_t) 5))
