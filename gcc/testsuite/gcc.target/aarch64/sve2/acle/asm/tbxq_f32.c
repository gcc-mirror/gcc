/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** tbx_f32_tied1:
**	tbx	z0\.s, z1\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (tbx_f32_tied1, svfloat32_t, svuint32_t,
	     z0 = svtbx_f32 (z0, z1, z4),
	     z0 = svtbx (z0, z1, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z (tbx_f32_tied2, svfloat32_t, svuint32_t,
	     z0 = svtbx_f32 (z1, z0, z4),
	     z0 = svtbx (z1, z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (tbx_f32_tied3, svfloat32_t, svuint32_t,
		 z0_res = svtbx_f32 (z4, z5, z0),
		 z0_res = svtbx (z4, z5, z0))

/*
** tbx_f32_untied:
** (
**	mov	z0\.d, z1\.d
**	tbx	z0\.s, z2\.s, z4\.s
** |
**	tbx	z1\.s, z2\.s, z4\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (tbx_f32_untied, svfloat32_t, svuint32_t,
	     z0 = svtbx_f32 (z1, z2, z4),
	     z0 = svtbx (z1, z2, z4))
