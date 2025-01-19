/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** tbx_u16_tied1:
**	tbx	z0\.h, z1\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (tbx_u16_tied1, svuint16_t, svuint16_t,
	     z0 = svtbx_u16 (z0, z1, z4),
	     z0 = svtbx (z0, z1, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z (tbx_u16_tied2, svuint16_t, svuint16_t,
	     z0 = svtbx_u16 (z1, z0, z4),
	     z0 = svtbx (z1, z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (tbx_u16_tied3, svuint16_t, svuint16_t,
		 z0_res = svtbx_u16 (z4, z5, z0),
		 z0_res = svtbx (z4, z5, z0))

/*
** tbx_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	tbx	z0\.h, z2\.h, z4\.h
** |
**	tbx	z1\.h, z2\.h, z4\.h
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (tbx_u16_untied, svuint16_t, svuint16_t,
	     z0 = svtbx_u16 (z1, z2, z4),
	     z0 = svtbx (z1, z2, z4))
