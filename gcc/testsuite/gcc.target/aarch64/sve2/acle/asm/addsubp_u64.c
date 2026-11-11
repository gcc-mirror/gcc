/* { dg-do assemble { target aarch64_asm_sve2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2p3"
#include "test_sve_acle.h"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2p3"
#endif

/*
** addsubp_u64_tied1:
**	addsubp	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (addsubp_u64_tied1, svuint64_t,
		z0 = svaddsubp_u64 (z0, z1),
		z0 = svaddsubp (z0, z1))

/*
** addsubp_u64_tied2:
**	addsubp	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (addsubp_u64_tied2, svuint64_t,
		z0 = svaddsubp_u64 (z1, z0),
		z0 = svaddsubp (z1, z0))

/*
** addsubp_u64_untied:
**	addsubp	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (addsubp_u64_untied, svuint64_t,
		z0 = svaddsubp_u64 (z1, z2),
		z0 = svaddsubp (z1, z2))
