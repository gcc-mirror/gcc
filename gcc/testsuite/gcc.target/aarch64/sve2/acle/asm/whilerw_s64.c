/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** whilerw_rr_s64:
**	whilerw	p0\.d, x0, x1
**	ret
*/
TEST_COMPARE_S (whilerw_rr_s64, const int64_t *,
		p0 = svwhilerw_s64 (x0, x1),
		p0 = svwhilerw (x0, x1))

/*
** whilerw_0r_s64:
**	whilerw	p0\.d, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilerw_0r_s64, const int64_t *,
		p0 = svwhilerw_s64 ((const int64_t *) 0, x1),
		p0 = svwhilerw ((const int64_t *) 0, x1))

/*
** whilerw_cr_s64:
**	mov	(x[0-9]+), #?1073741824
**	whilerw	p0\.d, \1, x1
**	ret
*/
TEST_COMPARE_S (whilerw_cr_s64, const int64_t *,
		p0 = svwhilerw_s64 ((const int64_t *) 1073741824, x1),
		p0 = svwhilerw ((const int64_t *) 1073741824, x1))

/*
** whilerw_r0_s64:
**	whilerw	p0\.d, x0, xzr
**	ret
*/
TEST_COMPARE_S (whilerw_r0_s64, const int64_t *,
		p0 = svwhilerw_s64 (x0, (const int64_t *) 0),
		p0 = svwhilerw (x0, (const int64_t *) 0))

/*
** whilerw_rc_s64:
**	mov	(x[0-9]+), #?1073741824
**	whilerw	p0\.d, x0, \1
**	ret
*/
TEST_COMPARE_S (whilerw_rc_s64, const int64_t *,
		p0 = svwhilerw_s64 (x0, (const int64_t *) 1073741824),
		p0 = svwhilerw (x0, (const int64_t *) 1073741824))
