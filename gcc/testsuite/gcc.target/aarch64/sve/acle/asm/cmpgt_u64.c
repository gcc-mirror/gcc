/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpgt_u64_tied:
** (
**	cmphi	p0\.d, p0/z, z0\.d, z1\.d
** |
**	cmplo	p0\.d, p0/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_u64_tied, svuint64_t,
		p0 = svcmpgt_u64 (p0, z0, z1),
		p0 = svcmpgt (p0, z0, z1))

/*
** cmpgt_u64_untied:
** (
**	cmphi	p0\.d, p1/z, z0\.d, z1\.d
** |
**	cmplo	p0\.d, p1/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_u64_untied, svuint64_t,
		p0 = svcmpgt_u64 (p1, z0, z1),
		p0 = svcmpgt (p1, z0, z1))

/*
** cmpgt_x0_u64:
**	mov	(z[0-9]+\.d), x0
** (
**	cmphi	p0\.d, p1/z, z0\.d, \1
** |
**	cmplo	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_ZX (cmpgt_x0_u64, svuint64_t, uint64_t,
		 p0 = svcmpgt_n_u64 (p1, z0, x0),
		 p0 = svcmpgt (p1, z0, x0))

/*
** cmpgt_0_u64:
**	cmphi	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmpgt_0_u64, svuint64_t,
		p0 = svcmpgt_n_u64 (p1, z0, 0),
		p0 = svcmpgt (p1, z0, 0))

/*
** cmpgt_1_u64:
**	cmphi	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmpgt_1_u64, svuint64_t,
		p0 = svcmpgt_n_u64 (p1, z0, 1),
		p0 = svcmpgt (p1, z0, 1))

/*
** cmpgt_15_u64:
**	cmphi	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmpgt_15_u64, svuint64_t,
		p0 = svcmpgt_n_u64 (p1, z0, 15),
		p0 = svcmpgt (p1, z0, 15))

/*
** cmpgt_16_u64:
**	cmphi	p0\.d, p1/z, z0\.d, #16
**	ret
*/
TEST_COMPARE_Z (cmpgt_16_u64, svuint64_t,
		p0 = svcmpgt_n_u64 (p1, z0, 16),
		p0 = svcmpgt (p1, z0, 16))

/*
** cmpgt_127_u64:
**	cmphi	p0\.d, p1/z, z0\.d, #127
**	ret
*/
TEST_COMPARE_Z (cmpgt_127_u64, svuint64_t,
		p0 = svcmpgt_n_u64 (p1, z0, 127),
		p0 = svcmpgt (p1, z0, 127))

/*
** cmpgt_128_u64:
**	mov	(z[0-9]+\.d), #128
** (
**	cmphi	p0\.d, p1/z, z0\.d, \1
** |
**	cmplo	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_128_u64, svuint64_t,
		p0 = svcmpgt_n_u64 (p1, z0, 128),
		p0 = svcmpgt (p1, z0, 128))

/*
** cmpgt_m1_u64:
**	mov	(z[0-9]+)\.b, #-1
** (
**	cmphi	p0\.d, p1/z, z0\.d, \1\.d
** |
**	cmplo	p0\.d, p1/z, \1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_m1_u64, svuint64_t,
		p0 = svcmpgt_n_u64 (p1, z0, -1),
		p0 = svcmpgt (p1, z0, -1))
