/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bdep_u32_tied1:
**	bdep	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (bdep_u32_tied1, svuint32_t,
		z0 = svbdep_u32 (z0, z1),
		z0 = svbdep (z0, z1))

/*
** bdep_u32_tied2:
**	bdep	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (bdep_u32_tied2, svuint32_t,
		z0 = svbdep_u32 (z1, z0),
		z0 = svbdep (z1, z0))

/*
** bdep_u32_untied:
**	bdep	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (bdep_u32_untied, svuint32_t,
		z0 = svbdep_u32 (z1, z2),
		z0 = svbdep (z1, z2))

/*
** bdep_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	bdep	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (bdep_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svbdep_n_u32 (z0, x0),
		 z0 = svbdep (z0, x0))

/*
** bdep_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	bdep	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (bdep_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svbdep_n_u32 (z1, x0),
		 z0 = svbdep (z1, x0))

/*
** bdep_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	bdep	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (bdep_11_u32_tied1, svuint32_t,
		z0 = svbdep_n_u32 (z0, 11),
		z0 = svbdep (z0, 11))

/*
** bdep_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
**	bdep	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (bdep_11_u32_untied, svuint32_t,
		z0 = svbdep_n_u32 (z1, 11),
		z0 = svbdep (z1, 11))
