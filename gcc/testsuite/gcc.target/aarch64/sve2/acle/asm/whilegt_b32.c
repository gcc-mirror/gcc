/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** whilegt_rr_b32_s32:
**	whilegt	p0\.s, w0, w1
**	ret
*/
TEST_COMPARE_S (whilegt_rr_b32_s32, int32_t,
		p0 = svwhilegt_b32_s32 (x0, x1),
		p0 = svwhilegt_b32 (x0, x1))

/*
** whilegt_0r_b32_s32:
**	whilegt	p0\.s, wzr, w1
**	ret
*/
TEST_COMPARE_S (whilegt_0r_b32_s32, int32_t,
		p0 = svwhilegt_b32_s32 (0, x1),
		p0 = svwhilegt_b32 (0, x1))

/*
** whilegt_5r_b32_s32:
**	mov	(w[0-9]+), #?5
**	whilegt	p0\.s, \1, w1
**	ret
*/
TEST_COMPARE_S (whilegt_5r_b32_s32, int32_t,
		p0 = svwhilegt_b32_s32 (5, x1),
		p0 = svwhilegt_b32 (5, x1))

/*
** whilegt_r0_b32_s32:
**	whilegt	p0\.s, w0, wzr
**	ret
*/
TEST_COMPARE_S (whilegt_r0_b32_s32, int32_t,
		p0 = svwhilegt_b32_s32 (x0, 0),
		p0 = svwhilegt_b32 (x0, 0))

/*
** whilegt_r5_b32_s32:
**	mov	(w[0-9]+), #?5
**	whilegt	p0\.s, w0, \1
**	ret
*/
TEST_COMPARE_S (whilegt_r5_b32_s32, int32_t,
		p0 = svwhilegt_b32_s32 (x0, 5),
		p0 = svwhilegt_b32 (x0, 5))

/*
** whilegt_rr_b32_s64:
**	whilegt	p0\.s, x0, x1
**	ret
*/
TEST_COMPARE_S (whilegt_rr_b32_s64, int64_t,
		p0 = svwhilegt_b32_s64 (x0, x1),
		p0 = svwhilegt_b32 (x0, x1))

/*
** whilegt_0r_b32_s64:
**	whilegt	p0\.s, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilegt_0r_b32_s64, int64_t,
		p0 = svwhilegt_b32_s64 (0, x1),
		p0 = svwhilegt_b32 ((int64_t) 0, x1))

/*
** whilegt_5r_b32_s64:
**	mov	(x[0-9]+), #?5
**	whilegt	p0\.s, \1, x1
**	ret
*/
TEST_COMPARE_S (whilegt_5r_b32_s64, int64_t,
		p0 = svwhilegt_b32_s64 (5, x1),
		p0 = svwhilegt_b32 ((int64_t) 5, x1))

/*
** whilegt_r0_b32_s64:
**	whilegt	p0\.s, x0, xzr
**	ret
*/
TEST_COMPARE_S (whilegt_r0_b32_s64, int64_t,
		p0 = svwhilegt_b32_s64 (x0, 0),
		p0 = svwhilegt_b32 (x0, (int64_t) 0))

/*
** whilegt_r5_b32_s64:
**	mov	(x[0-9]+), #?5
**	whilegt	p0\.s, x0, \1
**	ret
*/
TEST_COMPARE_S (whilegt_r5_b32_s64, int64_t,
		p0 = svwhilegt_b32_s64 (x0, 5),
		p0 = svwhilegt_b32 (x0, (int64_t) 5))

/*
** whilegt_rr_b32_u32:
**	whilehi	p0\.s, w0, w1
**	ret
*/
TEST_COMPARE_S (whilegt_rr_b32_u32, uint32_t,
		p0 = svwhilegt_b32_u32 (x0, x1),
		p0 = svwhilegt_b32 (x0, x1))

/*
** whilegt_0r_b32_u32:
**	whilehi	p0\.s, wzr, w1
**	ret
*/
TEST_COMPARE_S (whilegt_0r_b32_u32, uint32_t,
		p0 = svwhilegt_b32_u32 (0, x1),
		p0 = svwhilegt_b32 ((uint32_t) 0, x1))

/*
** whilegt_5r_b32_u32:
**	mov	(w[0-9]+), #?5
**	whilehi	p0\.s, \1, w1
**	ret
*/
TEST_COMPARE_S (whilegt_5r_b32_u32, uint32_t,
		p0 = svwhilegt_b32_u32 (5, x1),
		p0 = svwhilegt_b32 ((uint32_t) 5, x1))

/*
** whilegt_r5_b32_u32:
**	mov	(w[0-9]+), #?5
**	whilehi	p0\.s, w0, \1
**	ret
*/
TEST_COMPARE_S (whilegt_r5_b32_u32, uint32_t,
		p0 = svwhilegt_b32_u32 (x0, 5),
		p0 = svwhilegt_b32 (x0, (uint32_t) 5))

/*
** whilegt_rr_b32_u64:
**	whilehi	p0\.s, x0, x1
**	ret
*/
TEST_COMPARE_S (whilegt_rr_b32_u64, uint64_t,
		p0 = svwhilegt_b32_u64 (x0, x1),
		p0 = svwhilegt_b32 (x0, x1))

/*
** whilegt_0r_b32_u64:
**	whilehi	p0\.s, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilegt_0r_b32_u64, uint64_t,
		p0 = svwhilegt_b32_u64 (0, x1),
		p0 = svwhilegt_b32 ((uint64_t) 0, x1))

/*
** whilegt_5r_b32_u64:
**	mov	(x[0-9]+), #?5
**	whilehi	p0\.s, \1, x1
**	ret
*/
TEST_COMPARE_S (whilegt_5r_b32_u64, uint64_t,
		p0 = svwhilegt_b32_u64 (5, x1),
		p0 = svwhilegt_b32 ((uint64_t) 5, x1))

/*
** whilegt_r5_b32_u64:
**	mov	(x[0-9]+), #?5
**	whilehi	p0\.s, x0, \1
**	ret
*/
TEST_COMPARE_S (whilegt_r5_b32_u64, uint64_t,
		p0 = svwhilegt_b32_u64 (x0, 5),
		p0 = svwhilegt_b32 (x0, (uint64_t) 5))
