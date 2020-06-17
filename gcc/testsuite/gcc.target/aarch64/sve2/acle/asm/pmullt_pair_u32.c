/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pmullt_pair_u32_tied1:
**	pmullt	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_u32_tied1, svuint32_t,
		z0 = svpmullt_pair_u32 (z0, z1),
		z0 = svpmullt_pair (z0, z1))

/*
** pmullt_pair_u32_tied2:
**	pmullt	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_u32_tied2, svuint32_t,
		z0 = svpmullt_pair_u32 (z1, z0),
		z0 = svpmullt_pair (z1, z0))

/*
** pmullt_pair_u32_untied:
**	pmullt	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_u32_untied, svuint32_t,
		z0 = svpmullt_pair_u32 (z1, z2),
		z0 = svpmullt_pair (z1, z2))

/*
** pmullt_pair_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	pmullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullt_pair_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svpmullt_pair_n_u32 (z0, x0),
		 z0 = svpmullt_pair (z0, x0))

/*
** pmullt_pair_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	pmullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullt_pair_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svpmullt_pair_n_u32 (z1, x0),
		 z0 = svpmullt_pair (z1, x0))

/*
** pmullt_pair_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	pmullt	z0\.d, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_11_u32_tied1, svuint32_t,
		z0 = svpmullt_pair_n_u32 (z0, 11),
		z0 = svpmullt_pair (z0, 11))

/*
** pmullt_pair_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
**	pmullt	z0\.d, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_11_u32_untied, svuint32_t,
		z0 = svpmullt_pair_n_u32 (z1, 11),
		z0 = svpmullt_pair (z1, 11))
