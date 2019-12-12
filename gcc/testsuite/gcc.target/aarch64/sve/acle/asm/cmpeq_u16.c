/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpeq_u16_tied:
**	cmpeq	p0\.h, p0/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpeq_u16_tied, svuint16_t,
		p0 = svcmpeq_u16 (p0, z0, z1),
		p0 = svcmpeq (p0, z0, z1))

/*
** cmpeq_u16_untied:
**	cmpeq	p0\.h, p1/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpeq_u16_untied, svuint16_t,
		p0 = svcmpeq_u16 (p1, z0, z1),
		p0 = svcmpeq (p1, z0, z1))

/*
** cmpeq_w0_u16:
**	mov	(z[0-9]+\.h), w0
**	cmpeq	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_ZX (cmpeq_w0_u16, svuint16_t, uint16_t,
		 p0 = svcmpeq_n_u16 (p1, z0, x0),
		 p0 = svcmpeq (p1, z0, x0))

/*
** cmpeq_0_u16:
**	cmpeq	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmpeq_0_u16, svuint16_t,
		p0 = svcmpeq_n_u16 (p1, z0, 0),
		p0 = svcmpeq (p1, z0, 0))

/*
** cmpeq_1_u16:
**	cmpeq	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmpeq_1_u16, svuint16_t,
		p0 = svcmpeq_n_u16 (p1, z0, 1),
		p0 = svcmpeq (p1, z0, 1))

/*
** cmpeq_15_u16:
**	cmpeq	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmpeq_15_u16, svuint16_t,
		p0 = svcmpeq_n_u16 (p1, z0, 15),
		p0 = svcmpeq (p1, z0, 15))

/*
** cmpeq_16_u16:
**	mov	(z[0-9]+\.h), #16
**	cmpeq	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpeq_16_u16, svuint16_t,
		p0 = svcmpeq_n_u16 (p1, z0, 16),
		p0 = svcmpeq (p1, z0, 16))

/*
** cmpeq_m1_u16:
**	cmpeq	p0\.h, p1/z, z0\.h, #-1
**	ret
*/
TEST_COMPARE_Z (cmpeq_m1_u16, svuint16_t,
		p0 = svcmpeq_n_u16 (p1, z0, -1),
		p0 = svcmpeq (p1, z0, -1))

/*
** cmpeq_m16_u16:
**	cmpeq	p0\.h, p1/z, z0\.h, #-16
**	ret
*/
TEST_COMPARE_Z (cmpeq_m16_u16, svuint16_t,
		p0 = svcmpeq_n_u16 (p1, z0, -16),
		p0 = svcmpeq (p1, z0, -16))

/*
** cmpeq_m17_u16:
**	mov	(z[0-9]+\.h), #-17
**	cmpeq	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpeq_m17_u16, svuint16_t,
		p0 = svcmpeq_n_u16 (p1, z0, -17),
		p0 = svcmpeq (p1, z0, -17))
