/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** whilerw_rr_u64:
**	whilerw	p0\.d, x0, x1
**	ret
*/
TEST_COMPARE_S (whilerw_rr_u64, const uint64_t *,
		p0 = svwhilerw_u64 (x0, x1),
		p0 = svwhilerw (x0, x1))

/*
** whilerw_0r_u64:
**	whilerw	p0\.d, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilerw_0r_u64, const uint64_t *,
		p0 = svwhilerw_u64 ((const uint64_t *) 0, x1),
		p0 = svwhilerw ((const uint64_t *) 0, x1))

/*
** whilerw_cr_u64:
**	mov	(x[0-9]+), #?1073741824
**	whilerw	p0\.d, \1, x1
**	ret
*/
TEST_COMPARE_S (whilerw_cr_u64, const uint64_t *,
		p0 = svwhilerw_u64 ((const uint64_t *) 1073741824, x1),
		p0 = svwhilerw ((const uint64_t *) 1073741824, x1))

/*
** whilerw_r0_u64:
**	whilerw	p0\.d, x0, xzr
**	ret
*/
TEST_COMPARE_S (whilerw_r0_u64, const uint64_t *,
		p0 = svwhilerw_u64 (x0, (const uint64_t *) 0),
		p0 = svwhilerw (x0, (const uint64_t *) 0))

/*
** whilerw_rc_u64:
**	mov	(x[0-9]+), #?1073741824
**	whilerw	p0\.d, x0, \1
**	ret
*/
TEST_COMPARE_S (whilerw_rc_u64, const uint64_t *,
		p0 = svwhilerw_u64 (x0, (const uint64_t *) 1073741824),
		p0 = svwhilerw (x0, (const uint64_t *) 1073741824))
