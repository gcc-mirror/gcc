/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#pragma GCC target "+sve2p3"
#pragma GCC target "+sme2p3"

/*
** abal_u64_tied1:
**	uabal	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (abal_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svabal_u64 (z0, z4, z5),
	     z0 = svabal (z0, z4, z5))

/*
** abal_u64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uabal	z0\.d, \1\.s, z1\.s
**	ret
*/
TEST_DUAL_Z_REV (abal_u64_tied2, svuint64_t, svuint32_t,
		 z0_res = svabal_u64 (z4, z0, z1),
		 z0_res = svabal (z4, z0, z1))

/*
** abal_u64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uabal	z0\.d, z1\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (abal_u64_tied3, svuint64_t, svuint32_t,
		 z0_res = svabal_u64 (z4, z1, z0),
		 z0_res = svabal (z4, z1, z0))

/*
** abal_u64_untied:
**	movprfx	z0, z1
**	uabal	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (abal_u64_untied, svuint64_t, svuint32_t,
	     z0 = svabal_u64 (z1, z4, z5),
	     z0 = svabal (z1, z4, z5))

/*
** abal_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	uabal	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (abal_w0_u64_tied1, svuint64_t, svuint32_t, uint32_t,
	      z0 = svabal_n_u64 (z0, z4, x0),
	      z0 = svabal (z0, z4, x0))

/*
** abal_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	uabal	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (abal_w0_u64_untied, svuint64_t, svuint32_t, uint32_t,
	      z0 = svabal_n_u64 (z1, z4, x0),
	      z0 = svabal (z1, z4, x0))

/*
** abal_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	uabal	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (abal_11_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svabal_n_u64 (z0, z4, 11),
	     z0 = svabal (z0, z4, 11))

/*
** abal_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	uabal	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (abal_11_u64_untied, svuint64_t, svuint32_t,
	     z0 = svabal_n_u64 (z1, z4, 11),
	     z0 = svabal (z1, z4, 11))
