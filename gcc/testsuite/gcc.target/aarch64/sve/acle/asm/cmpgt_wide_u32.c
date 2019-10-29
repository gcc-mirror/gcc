/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpgt_wide_u32_tied:
**	cmphi	p0\.s, p0/z, z0\.s, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpgt_wide_u32_tied, svuint32_t, svuint64_t,
		     p0 = svcmpgt_wide_u32 (p0, z0, z1),
		     p0 = svcmpgt_wide (p0, z0, z1))

/*
** cmpgt_wide_u32_untied:
**	cmphi	p0\.s, p1/z, z0\.s, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpgt_wide_u32_untied, svuint32_t, svuint64_t,
		     p0 = svcmpgt_wide_u32 (p1, z0, z1),
		     p0 = svcmpgt_wide (p1, z0, z1))

/*
** cmpgt_wide_x0_u32:
**	mov	(z[0-9]+\.d), x0
**	cmphi	p0\.s, p1/z, z0\.s, \1
**	ret
*/
TEST_COMPARE_ZX (cmpgt_wide_x0_u32, svuint32_t, uint64_t,
		 p0 = svcmpgt_wide_n_u32 (p1, z0, x0),
		 p0 = svcmpgt_wide (p1, z0, x0))

/*
** cmpgt_wide_0_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #0
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_0_u32, svuint32_t,
		p0 = svcmpgt_wide_n_u32 (p1, z0, 0),
		p0 = svcmpgt_wide (p1, z0, 0))

/*
** cmpgt_wide_1_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #1
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_1_u32, svuint32_t,
		p0 = svcmpgt_wide_n_u32 (p1, z0, 1),
		p0 = svcmpgt_wide (p1, z0, 1))

/*
** cmpgt_wide_15_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #15
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_15_u32, svuint32_t,
		p0 = svcmpgt_wide_n_u32 (p1, z0, 15),
		p0 = svcmpgt_wide (p1, z0, 15))

/*
** cmpgt_wide_16_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #16
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_16_u32, svuint32_t,
		p0 = svcmpgt_wide_n_u32 (p1, z0, 16),
		p0 = svcmpgt_wide (p1, z0, 16))

/*
** cmpgt_wide_127_u32:
**	cmphi	p0\.s, p1/z, z0\.s, #127
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_127_u32, svuint32_t,
		p0 = svcmpgt_wide_n_u32 (p1, z0, 127),
		p0 = svcmpgt_wide (p1, z0, 127))

/*
** cmpgt_wide_128_u32:
**	mov	(z[0-9]+\.d), #128
**	cmphi	p0\.s, p1/z, z0\.s, \1
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_128_u32, svuint32_t,
		p0 = svcmpgt_wide_n_u32 (p1, z0, 128),
		p0 = svcmpgt_wide (p1, z0, 128))

/*
** cmpgt_wide_m1_u32:
**	mov	(z[0-9]+)\.b, #-1
**	cmphi	p0\.s, p1/z, z0\.s, \1\.d
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_m1_u32, svuint32_t,
		p0 = svcmpgt_wide_n_u32 (p1, z0, -1),
		p0 = svcmpgt_wide (p1, z0, -1))
