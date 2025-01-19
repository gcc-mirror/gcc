/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** zipq2_s32_tied1:
**	zipq2	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (zipq2_s32_tied1, svint32_t,
		z0 = svzipq2_s32 (z0, z1),
		z0 = svzipq2 (z0, z1))

/*
** zipq2_s32_tied2:
**	zipq2	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (zipq2_s32_tied2, svint32_t,
		z0 = svzipq2_s32 (z1, z0),
		z0 = svzipq2 (z1, z0))

/*
** zipq2_s32_untied:
**	zipq2	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (zipq2_s32_untied, svint32_t,
		z0 = svzipq2_s32 (z1, z2),
		z0 = svzipq2 (z1, z2))
