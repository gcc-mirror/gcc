/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** dot_u32_tied1:
**	udot	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (dot_u32_tied1, svuint32_t, svuint16_t,
	     z0 = svdot_u32_u16 (z0, z4, z5),
	     z0 = svdot (z0, z4, z5))

/*
** dot_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (dot_u32_tied2, svuint32_t, svuint16_t,
		 z0_res = svdot_u32_u16 (z4, z0, z1),
		 z0_res = svdot (z4, z0, z1))

/*
** dot_u32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (dot_u32_tied3, svuint32_t, svuint16_t,
		 z0_res = svdot_u32_u16 (z4, z1, z0),
		 z0_res = svdot (z4, z1, z0))

/*
** dot_u32_untied:
**	movprfx	z0, z1
**	udot	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (dot_u32_untied, svuint32_t, svuint16_t,
	     z0 = svdot_u32_u16 (z1, z4, z5),
	     z0 = svdot (z1, z4, z5))
