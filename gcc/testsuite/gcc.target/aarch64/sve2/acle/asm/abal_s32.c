/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#pragma GCC target "+sve2p3"
#pragma GCC target "+sme2p3"

/*
** abal_s32_tied1:
**	sabal	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (abal_s32_tied1, svint32_t, svint16_t,
	     z0 = svabal_s32 (z0, z4, z5),
	     z0 = svabal (z0, z4, z5))

/*
** abal_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sabal	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (abal_s32_tied2, svint32_t, svint16_t,
		 z0_res = svabal_s32 (z4, z0, z1),
		 z0_res = svabal (z4, z0, z1))

/*
** abal_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sabal	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (abal_s32_tied3, svint32_t, svint16_t,
		 z0_res = svabal_s32 (z4, z1, z0),
		 z0_res = svabal (z4, z1, z0))

/*
** abal_s32_untied:
**	movprfx	z0, z1
**	sabal	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (abal_s32_untied, svint32_t, svint16_t,
	     z0 = svabal_s32 (z1, z4, z5),
	     z0 = svabal (z1, z4, z5))

/*
** abal_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	sabal	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (abal_w0_s32_tied1, svint32_t, svint16_t, int16_t,
	      z0 = svabal_n_s32 (z0, z4, x0),
	      z0 = svabal (z0, z4, x0))

/*
** abal_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	sabal	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (abal_w0_s32_untied, svint32_t, svint16_t, int16_t,
	      z0 = svabal_n_s32 (z1, z4, x0),
	      z0 = svabal (z1, z4, x0))

/*
** abal_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	sabal	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (abal_11_s32_tied1, svint32_t, svint16_t,
	     z0 = svabal_n_s32 (z0, z4, 11),
	     z0 = svabal (z0, z4, 11))

/*
** abal_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	sabal	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (abal_11_s32_untied, svint32_t, svint16_t,
	     z0 = svabal_n_s32 (z1, z4, 11),
	     z0 = svabal (z1, z4, 11))
