/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpgt_u32_tied:
** (
**	cmphi	p0\.s, p0/z, z0\.s, z1\.s
** |
**	cmplo	p0\.s, p0/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_u32_tied, svuint32_t,
		p0 = svcmpgt_u32 (p0, z0, z1),
		p0 = svcmpgt (p0, z0, z1))

/*
** cmpgt_u32_untied:
** (
**	cmphi	p0\.s, p1/z, z0\.s, z1\.s
** |
**	cmplo	p0\.s, p1/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_u32_untied, svuint32_t,
		p0 = svcmpgt_u32 (p1, z0, z1),
		p0 = svcmpgt (p1, z0, z1))

/*
** cmpgt_w0_u32:
**	mov	(z[0-9]+\.s), w0
** (
**	cmphi	p0\.s, p1/z, z0\.s, \1
** |
**	cmplo	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_ZX (cmpgt_w0_u32, svuint32_t, uint32_t,
		 p0 = svcmpgt_n_u32 (p1, z0, x0),
		 p0 = svcmpgt (p1, z0, x0))

/*
** cmpgt_0_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #0
**	ret
*/
TEST_COMPARE_Z (cmpgt_0_u32, svuint32_t,
		p0 = svcmpgt_n_u32 (p1, z0, 0),
		p0 = svcmpgt (p1, z0, 0))

/*
** cmpgt_1_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #1
**	ret
*/
TEST_COMPARE_Z (cmpgt_1_u32, svuint32_t,
		p0 = svcmpgt_n_u32 (p1, z0, 1),
		p0 = svcmpgt (p1, z0, 1))

/*
** cmpgt_15_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #15
**	ret
*/
TEST_COMPARE_Z (cmpgt_15_u32, svuint32_t,
		p0 = svcmpgt_n_u32 (p1, z0, 15),
		p0 = svcmpgt (p1, z0, 15))

/*
** cmpgt_16_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #16
**	ret
*/
TEST_COMPARE_Z (cmpgt_16_u32, svuint32_t,
		p0 = svcmpgt_n_u32 (p1, z0, 16),
		p0 = svcmpgt (p1, z0, 16))

/*
** cmpgt_127_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #127
**	ret
*/
TEST_COMPARE_Z (cmpgt_127_u32, svuint32_t,
		p0 = svcmpgt_n_u32 (p1, z0, 127),
		p0 = svcmpgt (p1, z0, 127))

/*
** cmpgt_128_u32:
**	mov	(z[0-9]+\.s), #128
** (
**	cmphi	p0\.s, p1/z, z0\.s, \1
** |
**	cmplo	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_128_u32, svuint32_t,
		p0 = svcmpgt_n_u32 (p1, z0, 128),
		p0 = svcmpgt (p1, z0, 128))

/*
** cmpgt_m1_u32:
**	mov	(z[0-9]+)\.b, #-1
** (
**	cmphi	p0\.s, p1/z, z0\.s, \1\.s
** |
**	cmplo	p0\.s, p1/z, \1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_m1_u32, svuint32_t,
		p0 = svcmpgt_n_u32 (p1, z0, -1),
		p0 = svcmpgt (p1, z0, -1))
