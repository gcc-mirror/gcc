/* { dg-do assemble { target aarch64_asm_sve2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2p3"
#include "test_sve_acle.h"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2p3"
#endif

/*
** subp_u32_m_tied1:
**	subp	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (subp_u32_m_tied1, svuint32_t,
		z0 = svsubp_u32_m (p0, z0, z1),
		z0 = svsubp_m (p0, z0, z1))

/*
** subp_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	subp	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (subp_u32_m_tied2, svuint32_t,
		z0 = svsubp_u32_m (p0, z1, z0),
		z0 = svsubp_m (p0, z1, z0))

/*
** subp_u32_x_tied1:
**	subp	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (subp_u32_x_tied1, svuint32_t,
		z0 = svsubp_u32_x (p0, z0, z1),
		z0 = svsubp_x (p0, z0, z1))

/*
** subp_u32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	subp	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (subp_u32_x_tied2, svuint32_t,
		z0 = svsubp_u32_x (p0, z1, z0),
		z0 = svsubp_x (p0, z1, z0))

/*
** subp_u32_x_untied:
**	movprfx	z0, z1
**	subp	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (subp_u32_x_untied, svuint32_t,
		z0 = svsubp_u32_x (p0, z1, z2),
		z0 = svsubp_x (p0, z1, z2))
