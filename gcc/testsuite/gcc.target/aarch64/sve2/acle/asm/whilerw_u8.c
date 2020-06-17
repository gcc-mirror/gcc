/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** whilerw_rr_u8:
**	whilerw	p0\.b, x0, x1
**	ret
*/
TEST_COMPARE_S (whilerw_rr_u8, const uint8_t *,
		p0 = svwhilerw_u8 (x0, x1),
		p0 = svwhilerw (x0, x1))

/*
** whilerw_0r_u8:
**	whilerw	p0\.b, xzr, x1
**	ret
*/
TEST_COMPARE_S (whilerw_0r_u8, const uint8_t *,
		p0 = svwhilerw_u8 ((const uint8_t *) 0, x1),
		p0 = svwhilerw ((const uint8_t *) 0, x1))

/*
** whilerw_cr_u8:
**	mov	(x[0-9]+), #?1073741824
**	whilerw	p0\.b, \1, x1
**	ret
*/
TEST_COMPARE_S (whilerw_cr_u8, const uint8_t *,
		p0 = svwhilerw_u8 ((const uint8_t *) 1073741824, x1),
		p0 = svwhilerw ((const uint8_t *) 1073741824, x1))

/*
** whilerw_r0_u8:
**	whilerw	p0\.b, x0, xzr
**	ret
*/
TEST_COMPARE_S (whilerw_r0_u8, const uint8_t *,
		p0 = svwhilerw_u8 (x0, (const uint8_t *) 0),
		p0 = svwhilerw (x0, (const uint8_t *) 0))

/*
** whilerw_rc_u8:
**	mov	(x[0-9]+), #?1073741824
**	whilerw	p0\.b, x0, \1
**	ret
*/
TEST_COMPARE_S (whilerw_rc_u8, const uint8_t *,
		p0 = svwhilerw_u8 (x0, (const uint8_t *) 1073741824),
		p0 = svwhilerw (x0, (const uint8_t *) 1073741824))
