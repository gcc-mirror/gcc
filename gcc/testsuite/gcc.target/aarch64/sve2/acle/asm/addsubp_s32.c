/* { dg-do assemble { target aarch64_asm_sve2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2p3"
#include "test_sve_acle.h"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2p3"
#endif

/*
** addsubp_s32_tied1:
**	addsubp	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (addsubp_s32_tied1, svint32_t,
		z0 = svaddsubp_s32 (z0, z1),
		z0 = svaddsubp (z0, z1))

/*
** addsubp_s32_tied2:
**	addsubp	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (addsubp_s32_tied2, svint32_t,
		z0 = svaddsubp_s32 (z1, z0),
		z0 = svaddsubp (z1, z0))

/*
** addsubp_s32_untied:
**	addsubp	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (addsubp_s32_untied, svint32_t,
		z0 = svaddsubp_s32 (z1, z2),
		z0 = svaddsubp (z1, z2))
