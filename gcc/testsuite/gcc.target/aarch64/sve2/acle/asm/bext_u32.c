/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bext_u32_tied1:
**	bext	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (bext_u32_tied1, svuint32_t,
		z0 = svbext_u32 (z0, z1),
		z0 = svbext (z0, z1))

/*
** bext_u32_tied2:
**	bext	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (bext_u32_tied2, svuint32_t,
		z0 = svbext_u32 (z1, z0),
		z0 = svbext (z1, z0))

/*
** bext_u32_untied:
**	bext	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (bext_u32_untied, svuint32_t,
		z0 = svbext_u32 (z1, z2),
		z0 = svbext (z1, z2))

/*
** bext_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	bext	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (bext_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svbext_n_u32 (z0, x0),
		 z0 = svbext (z0, x0))

/*
** bext_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	bext	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (bext_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svbext_n_u32 (z1, x0),
		 z0 = svbext (z1, x0))

/*
** bext_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	bext	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (bext_11_u32_tied1, svuint32_t,
		z0 = svbext_n_u32 (z0, 11),
		z0 = svbext (z0, 11))

/*
** bext_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
**	bext	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (bext_11_u32_untied, svuint32_t,
		z0 = svbext_n_u32 (z1, 11),
		z0 = svbext (z1, 11))
