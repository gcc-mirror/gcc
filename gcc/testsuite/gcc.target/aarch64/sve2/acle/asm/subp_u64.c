/* { dg-do assemble { target aarch64_asm_sve2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2p3"
#include "test_sve_acle.h"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2p3"
#endif

/*
** subp_u64_m_tied1:
**	subp	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (subp_u64_m_tied1, svuint64_t,
		z0 = svsubp_u64_m (p0, z0, z1),
		z0 = svsubp_m (p0, z0, z1))

/*
** subp_u64_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	subp	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (subp_u64_m_tied2, svuint64_t,
		z0 = svsubp_u64_m (p0, z1, z0),
		z0 = svsubp_m (p0, z1, z0))

/*
** subp_u64_x_tied1:
**	subp	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (subp_u64_x_tied1, svuint64_t,
		z0 = svsubp_u64_x (p0, z0, z1),
		z0 = svsubp_x (p0, z0, z1))

/*
** subp_u64_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	subp	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (subp_u64_x_tied2, svuint64_t,
		z0 = svsubp_u64_x (p0, z1, z0),
		z0 = svsubp_x (p0, z1, z0))

/*
** subp_u64_x_untied:
**	movprfx	z0, z1
**	subp	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (subp_u64_x_untied, svuint64_t,
		z0 = svsubp_u64_x (p0, z1, z2),
		z0 = svsubp_x (p0, z1, z2))
