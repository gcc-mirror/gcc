/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#pragma GCC target "+sve2p3"
#pragma GCC target "+sme2p3"

/*
** abal_s64_tied1:
**	sabal	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (abal_s64_tied1, svint64_t, svint32_t,
	     z0 = svabal_s64 (z0, z4, z5),
	     z0 = svabal (z0, z4, z5))

/*
** abal_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sabal	z0\.d, \1\.s, z1\.s
**	ret
*/
TEST_DUAL_Z_REV (abal_s64_tied2, svint64_t, svint32_t,
		 z0_res = svabal_s64 (z4, z0, z1),
		 z0_res = svabal (z4, z0, z1))

/*
** abal_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sabal	z0\.d, z1\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (abal_s64_tied3, svint64_t, svint32_t,
		 z0_res = svabal_s64 (z4, z1, z0),
		 z0_res = svabal (z4, z1, z0))

/*
** abal_s64_untied:
**	movprfx	z0, z1
**	sabal	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (abal_s64_untied, svint64_t, svint32_t,
	     z0 = svabal_s64 (z1, z4, z5),
	     z0 = svabal (z1, z4, z5))

/*
** abal_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	sabal	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (abal_w0_s64_tied1, svint64_t, svint32_t, int32_t,
	      z0 = svabal_n_s64 (z0, z4, x0),
	      z0 = svabal (z0, z4, x0))

/*
** abal_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	sabal	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (abal_w0_s64_untied, svint64_t, svint32_t, int32_t,
	      z0 = svabal_n_s64 (z1, z4, x0),
	      z0 = svabal (z1, z4, x0))

/*
** abal_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	sabal	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (abal_11_s64_tied1, svint64_t, svint32_t,
	     z0 = svabal_n_s64 (z0, z4, 11),
	     z0 = svabal (z0, z4, 11))

/*
** abal_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	sabal	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (abal_11_s64_untied, svint64_t, svint32_t,
	     z0 = svabal_n_s64 (z1, z4, 11),
	     z0 = svabal (z1, z4, 11))
