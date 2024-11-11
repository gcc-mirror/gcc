/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** clamp_f64_tied1:
**	fclamp	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (clamp_f64_tied1, svfloat64_t,
		z0 = svclamp_f64 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_f64_tied2:
**	fclamp	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (clamp_f64_tied2, svfloat64_t,
		z0 = svclamp_f64 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_f64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fclamp	z0\.d, z2\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (clamp_f64_tied3, svfloat64_t,
		z0 = svclamp_f64 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_f64_untied:
**	movprfx	z0, z1
**	fclamp	z0\.d, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (clamp_f64_untied, svfloat64_t,
		z0 = svclamp_f64 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
