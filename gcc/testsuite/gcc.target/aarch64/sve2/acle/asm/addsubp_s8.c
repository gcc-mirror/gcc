/* { dg-do assemble { target aarch64_asm_sve2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2p3"
#include "test_sve_acle.h"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2p3"
#endif

/*
** addsubp_s8_tied1:
**	addsubp	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (addsubp_s8_tied1, svint8_t,
		z0 = svaddsubp_s8 (z0, z1),
		z0 = svaddsubp (z0, z1))

/*
** addsubp_s8_tied2:
**	addsubp	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (addsubp_s8_tied2, svint8_t,
		z0 = svaddsubp_s8 (z1, z0),
		z0 = svaddsubp (z1, z0))

/*
** addsubp_s8_untied:
**	addsubp	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (addsubp_s8_untied, svint8_t,
		z0 = svaddsubp_s8 (z1, z2),
		z0 = svaddsubp (z1, z2))
