/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bdep_u16_tied1:
**	bdep	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (bdep_u16_tied1, svuint16_t,
		z0 = svbdep_u16 (z0, z1),
		z0 = svbdep (z0, z1))

/*
** bdep_u16_tied2:
**	bdep	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (bdep_u16_tied2, svuint16_t,
		z0 = svbdep_u16 (z1, z0),
		z0 = svbdep (z1, z0))

/*
** bdep_u16_untied:
**	bdep	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (bdep_u16_untied, svuint16_t,
		z0 = svbdep_u16 (z1, z2),
		z0 = svbdep (z1, z2))

/*
** bdep_w0_u16_tied1:
**	mov	(z[0-9]+\.h), w0
**	bdep	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bdep_w0_u16_tied1, svuint16_t, uint16_t,
		 z0 = svbdep_n_u16 (z0, x0),
		 z0 = svbdep (z0, x0))

/*
** bdep_w0_u16_untied:
**	mov	(z[0-9]+\.h), w0
**	bdep	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bdep_w0_u16_untied, svuint16_t, uint16_t,
		 z0 = svbdep_n_u16 (z1, x0),
		 z0 = svbdep (z1, x0))

/*
** bdep_11_u16_tied1:
**	mov	(z[0-9]+\.h), #11
**	bdep	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bdep_11_u16_tied1, svuint16_t,
		z0 = svbdep_n_u16 (z0, 11),
		z0 = svbdep (z0, 11))

/*
** bdep_11_u16_untied:
**	mov	(z[0-9]+\.h), #11
**	bdep	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bdep_11_u16_untied, svuint16_t,
		z0 = svbdep_n_u16 (z1, 11),
		z0 = svbdep (z1, 11))
