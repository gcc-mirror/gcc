/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmple_u32_tied:
** (
**	cmphs	p0\.s, p0/z, z1\.s, z0\.s
** |
**	cmpls	p0\.s, p0/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmple_u32_tied, svuint32_t,
		p0 = svcmple_u32 (p0, z0, z1),
		p0 = svcmple (p0, z0, z1))

/*
** cmple_u32_untied:
** (
**	cmphs	p0\.s, p1/z, z1\.s, z0\.s
** |
**	cmpls	p0\.s, p1/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmple_u32_untied, svuint32_t,
		p0 = svcmple_u32 (p1, z0, z1),
		p0 = svcmple (p1, z0, z1))

/*
** cmple_w0_u32:
**	mov	(z[0-9]+\.s), w0
** (
**	cmphs	p0\.s, p1/z, \1, z0\.s
** |
**	cmpls	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_ZX (cmple_w0_u32, svuint32_t, uint32_t,
		 p0 = svcmple_n_u32 (p1, z0, x0),
		 p0 = svcmple (p1, z0, x0))

/*
** cmple_0_u32:
**	cmpls	p0\.s, p1/z, z0\.s, #0
**	ret
*/
TEST_COMPARE_Z (cmple_0_u32, svuint32_t,
		p0 = svcmple_n_u32 (p1, z0, 0),
		p0 = svcmple (p1, z0, 0))

/*
** cmple_1_u32:
**	cmpls	p0\.s, p1/z, z0\.s, #1
**	ret
*/
TEST_COMPARE_Z (cmple_1_u32, svuint32_t,
		p0 = svcmple_n_u32 (p1, z0, 1),
		p0 = svcmple (p1, z0, 1))

/*
** cmple_15_u32:
**	cmpls	p0\.s, p1/z, z0\.s, #15
**	ret
*/
TEST_COMPARE_Z (cmple_15_u32, svuint32_t,
		p0 = svcmple_n_u32 (p1, z0, 15),
		p0 = svcmple (p1, z0, 15))

/*
** cmple_16_u32:
**	cmpls	p0\.s, p1/z, z0\.s, #16
**	ret
*/
TEST_COMPARE_Z (cmple_16_u32, svuint32_t,
		p0 = svcmple_n_u32 (p1, z0, 16),
		p0 = svcmple (p1, z0, 16))

/*
** cmple_127_u32:
**	cmpls	p0\.s, p1/z, z0\.s, #127
**	ret
*/
TEST_COMPARE_Z (cmple_127_u32, svuint32_t,
		p0 = svcmple_n_u32 (p1, z0, 127),
		p0 = svcmple (p1, z0, 127))

/*
** cmple_128_u32:
**	mov	(z[0-9]+\.s), #128
** (
**	cmphs	p0\.s, p1/z, \1, z0\.s
** |
**	cmpls	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmple_128_u32, svuint32_t,
		p0 = svcmple_n_u32 (p1, z0, 128),
		p0 = svcmple (p1, z0, 128))

/*
** cmple_m1_u32:
**	mov	(z[0-9]+)\.b, #-1
** (
**	cmphs	p0\.s, p1/z, \1\.s, z0\.s
** |
**	cmpls	p0\.s, p1/z, z0\.s, \1\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmple_m1_u32, svuint32_t,
		p0 = svcmple_n_u32 (p1, z0, -1),
		p0 = svcmple (p1, z0, -1))
