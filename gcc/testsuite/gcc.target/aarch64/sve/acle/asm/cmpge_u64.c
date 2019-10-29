/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_u64_tied:
** (
**	cmphs	p0\.d, p0/z, z0\.d, z1\.d
** |
**	cmpls	p0\.d, p0/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_u64_tied, svuint64_t,
		p0 = svcmpge_u64 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_u64_untied:
** (
**	cmphs	p0\.d, p1/z, z0\.d, z1\.d
** |
**	cmpls	p0\.d, p1/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_u64_untied, svuint64_t,
		p0 = svcmpge_u64 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_x0_u64:
**	mov	(z[0-9]+\.d), x0
** (
**	cmphs	p0\.d, p1/z, z0\.d, \1
** |
**	cmpls	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_ZX (cmpge_x0_u64, svuint64_t, uint64_t,
		 p0 = svcmpge_n_u64 (p1, z0, x0),
		 p0 = svcmpge (p1, z0, x0))

/*
** cmpge_0_u64:
**	cmphs	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_u64, svuint64_t,
		p0 = svcmpge_n_u64 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_u64:
**	cmphs	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmpge_1_u64, svuint64_t,
		p0 = svcmpge_n_u64 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))

/*
** cmpge_15_u64:
**	cmphs	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmpge_15_u64, svuint64_t,
		p0 = svcmpge_n_u64 (p1, z0, 15),
		p0 = svcmpge (p1, z0, 15))

/*
** cmpge_16_u64:
**	cmphs	p0\.d, p1/z, z0\.d, #16
**	ret
*/
TEST_COMPARE_Z (cmpge_16_u64, svuint64_t,
		p0 = svcmpge_n_u64 (p1, z0, 16),
		p0 = svcmpge (p1, z0, 16))

/*
** cmpge_127_u64:
**	cmphs	p0\.d, p1/z, z0\.d, #127
**	ret
*/
TEST_COMPARE_Z (cmpge_127_u64, svuint64_t,
		p0 = svcmpge_n_u64 (p1, z0, 127),
		p0 = svcmpge (p1, z0, 127))

/*
** cmpge_128_u64:
**	mov	(z[0-9]+\.d), #128
** (
**	cmphs	p0\.d, p1/z, z0\.d, \1
** |
**	cmpls	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_128_u64, svuint64_t,
		p0 = svcmpge_n_u64 (p1, z0, 128),
		p0 = svcmpge (p1, z0, 128))

/*
** cmpge_m1_u64:
**	mov	(z[0-9]+)\.b, #-1
** (
**	cmphs	p0\.d, p1/z, z0\.d, \1\.d
** |
**	cmpls	p0\.d, p1/z, \1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_m1_u64, svuint64_t,
		p0 = svcmpge_n_u64 (p1, z0, -1),
		p0 = svcmpge (p1, z0, -1))
