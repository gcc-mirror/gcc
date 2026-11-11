/* { dg-do assemble { target aarch64_asm_sve2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2p3"
#include "test_sve_acle.h"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2p3"
#endif

/*
** addsubp_s16_tied1:
**	addsubp	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (addsubp_s16_tied1, svint16_t,
		z0 = svaddsubp_s16 (z0, z1),
		z0 = svaddsubp (z0, z1))

/*
** addsubp_s16_tied2:
**	addsubp	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (addsubp_s16_tied2, svint16_t,
		z0 = svaddsubp_s16 (z1, z0),
		z0 = svaddsubp (z1, z0))

/*
** addsubp_s16_untied:
**	addsubp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (addsubp_s16_untied, svint16_t,
		z0 = svaddsubp_s16 (z1, z2),
		z0 = svaddsubp (z1, z2))
