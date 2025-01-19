/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** revd_f16_m_tied12:
**	revd	z0\.q, p0/m, z0\.q
**	ret
*/
TEST_UNIFORM_Z (revd_f16_m_tied12, svfloat16_t,
		z0 = svrevd_f16_m (z0, p0, z0),
		z0 = svrevd_m (z0, p0, z0))

/*
** revd_f16_m_tied1:
**	revd	z0\.q, p0/m, z1\.q
**	ret
*/
TEST_UNIFORM_Z (revd_f16_m_tied1, svfloat16_t,
		z0 = svrevd_f16_m (z0, p0, z1),
		z0 = svrevd_m (z0, p0, z1))

/*
** revd_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	revd	z0\.q, p0/m, \1\.q
**	ret
*/
TEST_UNIFORM_Z (revd_f16_m_tied2, svfloat16_t,
		z0 = svrevd_f16_m (z1, p0, z0),
		z0 = svrevd_m (z1, p0, z0))

/*
** revd_f16_m_untied:
**	movprfx	z0, z2
**	revd	z0\.q, p0/m, z1\.q
**	ret
*/
TEST_UNIFORM_Z (revd_f16_m_untied, svfloat16_t,
		z0 = svrevd_f16_m (z2, p0, z1),
		z0 = svrevd_m (z2, p0, z1))

/* Awkward register allocation.  Don't require specific output.  */
TEST_UNIFORM_Z (revd_f16_z_tied1, svfloat16_t,
		z0 = svrevd_f16_z (p0, z0),
		z0 = svrevd_z (p0, z0))

/*
** revd_f16_z_untied:
**	movi?	[vdz]0\.?(?:[0-9]*[bhsd])?, #?0
**	revd	z0\.q, p0/m, z1\.q
**	ret
*/
TEST_UNIFORM_Z (revd_f16_z_untied, svfloat16_t,
		z0 = svrevd_f16_z (p0, z1),
		z0 = svrevd_z (p0, z1))

/*
** revd_f16_x_tied1:
**	revd	z0\.q, p0/m, z0\.q
**	ret
*/
TEST_UNIFORM_Z (revd_f16_x_tied1, svfloat16_t,
		z0 = svrevd_f16_x (p0, z0),
		z0 = svrevd_x (p0, z0))

/*
** revd_f16_x_untied:
**	movprfx	z0, z1
**	revd	z0\.q, p0/m, z1\.q
**	ret
*/
TEST_UNIFORM_Z (revd_f16_x_untied, svfloat16_t,
		z0 = svrevd_f16_x (p0, z1),
		z0 = svrevd_x (p0, z1))
