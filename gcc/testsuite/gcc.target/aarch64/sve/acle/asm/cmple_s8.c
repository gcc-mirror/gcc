/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmple_s8_tied:
** (
**	cmpge	p0\.b, p0/z, z1\.b, z0\.b
** |
**	cmple	p0\.b, p0/z, z0\.b, z1\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmple_s8_tied, svint8_t,
		p0 = svcmple_s8 (p0, z0, z1),
		p0 = svcmple (p0, z0, z1))

/*
** cmple_s8_untied:
** (
**	cmpge	p0\.b, p1/z, z1\.b, z0\.b
** |
**	cmple	p0\.b, p1/z, z0\.b, z1\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmple_s8_untied, svint8_t,
		p0 = svcmple_s8 (p1, z0, z1),
		p0 = svcmple (p1, z0, z1))

/*
** cmple_w0_s8:
**	mov	(z[0-9]+\.b), w0
** (
**	cmpge	p0\.b, p1/z, \1, z0\.b
** |
**	cmple	p0\.b, p1/z, z0\.b, \1
** )
**	ret
*/
TEST_COMPARE_ZX (cmple_w0_s8, svint8_t, int8_t,
		 p0 = svcmple_n_s8 (p1, z0, x0),
		 p0 = svcmple (p1, z0, x0))

/*
** cmple_0_s8:
**	cmple	p0\.b, p1/z, z0\.b, #0
**	ret
*/
TEST_COMPARE_Z (cmple_0_s8, svint8_t,
		p0 = svcmple_n_s8 (p1, z0, 0),
		p0 = svcmple (p1, z0, 0))

/*
** cmple_1_s8:
**	cmple	p0\.b, p1/z, z0\.b, #1
**	ret
*/
TEST_COMPARE_Z (cmple_1_s8, svint8_t,
		p0 = svcmple_n_s8 (p1, z0, 1),
		p0 = svcmple (p1, z0, 1))

/*
** cmple_15_s8:
**	cmple	p0\.b, p1/z, z0\.b, #15
**	ret
*/
TEST_COMPARE_Z (cmple_15_s8, svint8_t,
		p0 = svcmple_n_s8 (p1, z0, 15),
		p0 = svcmple (p1, z0, 15))

/*
** cmple_16_s8:
**	mov	(z[0-9]+\.b), #16
** (
**	cmpge	p0\.b, p1/z, \1, z0\.b
** |
**	cmple	p0\.b, p1/z, z0\.b, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmple_16_s8, svint8_t,
		p0 = svcmple_n_s8 (p1, z0, 16),
		p0 = svcmple (p1, z0, 16))

/*
** cmple_m1_s8:
**	cmple	p0\.b, p1/z, z0\.b, #-1
**	ret
*/
TEST_COMPARE_Z (cmple_m1_s8, svint8_t,
		p0 = svcmple_n_s8 (p1, z0, -1),
		p0 = svcmple (p1, z0, -1))

/*
** cmple_m16_s8:
**	cmple	p0\.b, p1/z, z0\.b, #-16
**	ret
*/
TEST_COMPARE_Z (cmple_m16_s8, svint8_t,
		p0 = svcmple_n_s8 (p1, z0, -16),
		p0 = svcmple (p1, z0, -16))

/*
** cmple_m17_s8:
**	mov	(z[0-9]+\.b), #-17
** (
**	cmpge	p0\.b, p1/z, \1, z0\.b
** |
**	cmple	p0\.b, p1/z, z0\.b, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmple_m17_s8, svint8_t,
		p0 = svcmple_n_s8 (p1, z0, -17),
		p0 = svcmple (p1, z0, -17))
