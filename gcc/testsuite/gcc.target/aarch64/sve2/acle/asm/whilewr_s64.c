/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** whilewr_rr_s64:
**	whilewr	p0\.d, x0, x1
**	ret
*/
TEST_COMPARE_S (whilewr_rr_s64, const int64_t *,
		p0 = svwhilewr_s64 (x0, x1),
		p0 = svwhilewr (x0, x1))

/*
** whilewr_0r_s64:
**	whilewr	p0\.d, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilewr_0r_s64, const int64_t *,
		p0 = svwhilewr_s64 ((const int64_t *) 0, x1),
		p0 = svwhilewr ((const int64_t *) 0, x1))

/*
** whilewr_cr_s64:
**	mov	(x[0-9]+), #?1073741824
**	whilewr	p0\.d, \1, x1
**	ret
*/
TEST_COMPARE_S (whilewr_cr_s64, const int64_t *,
		p0 = svwhilewr_s64 ((const int64_t *) 1073741824, x1),
		p0 = svwhilewr ((const int64_t *) 1073741824, x1))

/*
** whilewr_r0_s64:
**	whilewr	p0\.d, x0, xzr
**	ret
*/
TEST_COMPARE_S (whilewr_r0_s64, const int64_t *,
		p0 = svwhilewr_s64 (x0, (const int64_t *) 0),
		p0 = svwhilewr (x0, (const int64_t *) 0))

/*
** whilewr_rc_s64:
**	mov	(x[0-9]+), #?1073741824
**	whilewr	p0\.d, x0, \1
**	ret
*/
TEST_COMPARE_S (whilewr_rc_s64, const int64_t *,
		p0 = svwhilewr_s64 (x0, (const int64_t *) 1073741824),
		p0 = svwhilewr (x0, (const int64_t *) 1073741824))
