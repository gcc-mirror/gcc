/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pmullb_pair_u32_tied1:
**	pmullb	z0\.d, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_u32_tied1, svuint32_t,
		z0 = svpmullb_pair_u32 (z0, z1),
		z0 = svpmullb_pair (z0, z1))

/*
** pmullb_pair_u32_tied2:
**	pmullb	z0\.d, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_u32_tied2, svuint32_t,
		z0 = svpmullb_pair_u32 (z1, z0),
		z0 = svpmullb_pair (z1, z0))

/*
** pmullb_pair_u32_untied:
**	pmullb	z0\.d, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_u32_untied, svuint32_t,
		z0 = svpmullb_pair_u32 (z1, z2),
		z0 = svpmullb_pair (z1, z2))

/*
** pmullb_pair_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	pmullb	z0\.d, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullb_pair_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svpmullb_pair_n_u32 (z0, x0),
		 z0 = svpmullb_pair (z0, x0))

/*
** pmullb_pair_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	pmullb	z0\.d, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullb_pair_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svpmullb_pair_n_u32 (z1, x0),
		 z0 = svpmullb_pair (z1, x0))

/*
** pmullb_pair_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	pmullb	z0\.d, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_11_u32_tied1, svuint32_t,
		z0 = svpmullb_pair_n_u32 (z0, 11),
		z0 = svpmullb_pair (z0, 11))

/*
** pmullb_pair_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
**	pmullb	z0\.d, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (pmullb_pair_11_u32_untied, svuint32_t,
		z0 = svpmullb_pair_n_u32 (z1, 11),
		z0 = svpmullb_pair (z1, 11))
