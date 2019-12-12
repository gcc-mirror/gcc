/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmple_u64_tied:
** (
**	cmphs	p0\.d, p0/z, z1\.d, z0\.d
** |
**	cmpls	p0\.d, p0/z, z0\.d, z1\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmple_u64_tied, svuint64_t,
		p0 = svcmple_u64 (p0, z0, z1),
		p0 = svcmple (p0, z0, z1))

/*
** cmple_u64_untied:
** (
**	cmphs	p0\.d, p1/z, z1\.d, z0\.d
** |
**	cmpls	p0\.d, p1/z, z0\.d, z1\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmple_u64_untied, svuint64_t,
		p0 = svcmple_u64 (p1, z0, z1),
		p0 = svcmple (p1, z0, z1))

/*
** cmple_x0_u64:
**	mov	(z[0-9]+\.d), x0
** (
**	cmphs	p0\.d, p1/z, \1, z0\.d
** |
**	cmpls	p0\.d, p1/z, z0\.d, \1
** )
**	ret
*/
TEST_COMPARE_ZX (cmple_x0_u64, svuint64_t, uint64_t,
		 p0 = svcmple_n_u64 (p1, z0, x0),
		 p0 = svcmple (p1, z0, x0))

/*
** cmple_0_u64:
**	cmpls	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmple_0_u64, svuint64_t,
		p0 = svcmple_n_u64 (p1, z0, 0),
		p0 = svcmple (p1, z0, 0))

/*
** cmple_1_u64:
**	cmpls	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmple_1_u64, svuint64_t,
		p0 = svcmple_n_u64 (p1, z0, 1),
		p0 = svcmple (p1, z0, 1))

/*
** cmple_15_u64:
**	cmpls	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmple_15_u64, svuint64_t,
		p0 = svcmple_n_u64 (p1, z0, 15),
		p0 = svcmple (p1, z0, 15))

/*
** cmple_16_u64:
**	cmpls	p0\.d, p1/z, z0\.d, #16
**	ret
*/
TEST_COMPARE_Z (cmple_16_u64, svuint64_t,
		p0 = svcmple_n_u64 (p1, z0, 16),
		p0 = svcmple (p1, z0, 16))

/*
** cmple_127_u64:
**	cmpls	p0\.d, p1/z, z0\.d, #127
**	ret
*/
TEST_COMPARE_Z (cmple_127_u64, svuint64_t,
		p0 = svcmple_n_u64 (p1, z0, 127),
		p0 = svcmple (p1, z0, 127))

/*
** cmple_128_u64:
**	mov	(z[0-9]+\.d), #128
** (
**	cmphs	p0\.d, p1/z, \1, z0\.d
** |
**	cmpls	p0\.d, p1/z, z0\.d, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmple_128_u64, svuint64_t,
		p0 = svcmple_n_u64 (p1, z0, 128),
		p0 = svcmple (p1, z0, 128))

/*
** cmple_m1_u64:
**	mov	(z[0-9]+)\.b, #-1
** (
**	cmphs	p0\.d, p1/z, \1\.d, z0\.d
** |
**	cmpls	p0\.d, p1/z, z0\.d, \1\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmple_m1_u64, svuint64_t,
		p0 = svcmple_n_u64 (p1, z0, -1),
		p0 = svcmple (p1, z0, -1))
