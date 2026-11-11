/* { dg-do assemble { target aarch64_asm_sve2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2p3"
#include "test_sve_acle.h"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2p3"
#endif

/*
** addqp_u8_tied1:
**	addqp	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (addqp_u8_tied1, svuint8_t,
		z0 = svaddqp_u8 (z0, z1),
		z0 = svaddqp (z0, z1))

/*
** addqp_u8_tied2:
**	addqp	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (addqp_u8_tied2, svuint8_t,
		z0 = svaddqp_u8 (z1, z0),
		z0 = svaddqp (z1, z0))

/*
** addqp_u8_untied:
**	addqp	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (addqp_u8_untied, svuint8_t,
		z0 = svaddqp_u8 (z1, z2),
		z0 = svaddqp (z1, z2))
