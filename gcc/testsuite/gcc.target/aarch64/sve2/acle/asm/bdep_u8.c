/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bdep_u8_tied1:
**	bdep	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (bdep_u8_tied1, svuint8_t,
		z0 = svbdep_u8 (z0, z1),
		z0 = svbdep (z0, z1))

/*
** bdep_u8_tied2:
**	bdep	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (bdep_u8_tied2, svuint8_t,
		z0 = svbdep_u8 (z1, z0),
		z0 = svbdep (z1, z0))

/*
** bdep_u8_untied:
**	bdep	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (bdep_u8_untied, svuint8_t,
		z0 = svbdep_u8 (z1, z2),
		z0 = svbdep (z1, z2))

/*
** bdep_w0_u8_tied1:
**	mov	(z[0-9]+\.b), w0
**	bdep	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bdep_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = svbdep_n_u8 (z0, x0),
		 z0 = svbdep (z0, x0))

/*
** bdep_w0_u8_untied:
**	mov	(z[0-9]+\.b), w0
**	bdep	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bdep_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = svbdep_n_u8 (z1, x0),
		 z0 = svbdep (z1, x0))

/*
** bdep_11_u8_tied1:
**	mov	(z[0-9]+\.b), #11
**	bdep	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bdep_11_u8_tied1, svuint8_t,
		z0 = svbdep_n_u8 (z0, 11),
		z0 = svbdep (z0, 11))

/*
** bdep_11_u8_untied:
**	mov	(z[0-9]+\.b), #11
**	bdep	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bdep_11_u8_untied, svuint8_t,
		z0 = svbdep_n_u8 (z1, 11),
		z0 = svbdep (z1, 11))
