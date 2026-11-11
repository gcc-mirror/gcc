/* { dg-do assemble { target aarch64_asm_sve2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2p3"
#include "test_sve_acle.h"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2p3"
#endif

/*
** subp_u8_m_tied1:
**	subp	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (subp_u8_m_tied1, svuint8_t,
		z0 = svsubp_u8_m (p0, z0, z1),
		z0 = svsubp_m (p0, z0, z1))

/*
** subp_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	subp	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (subp_u8_m_tied2, svuint8_t,
		z0 = svsubp_u8_m (p0, z1, z0),
		z0 = svsubp_m (p0, z1, z0))

/*
** subp_u8_x_tied1:
**	subp	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (subp_u8_x_tied1, svuint8_t,
		z0 = svsubp_u8_x (p0, z0, z1),
		z0 = svsubp_x (p0, z0, z1))

/*
** subp_u8_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	subp	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (subp_u8_x_tied2, svuint8_t,
		z0 = svsubp_u8_x (p0, z1, z0),
		z0 = svsubp_x (p0, z1, z0))

/*
** subp_u8_x_untied:
**	movprfx	z0, z1
**	subp	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (subp_u8_x_untied, svuint8_t,
		z0 = svsubp_u8_x (p0, z1, z2),
		z0 = svsubp_x (p0, z1, z2))
