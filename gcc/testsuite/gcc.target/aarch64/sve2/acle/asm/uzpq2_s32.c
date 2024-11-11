/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** uzpq2_s32_tied1:
**	uzpq2	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (uzpq2_s32_tied1, svint32_t,
		z0 = svuzpq2_s32 (z0, z1),
		z0 = svuzpq2 (z0, z1))

/*
** uzpq2_s32_tied2:
**	uzpq2	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (uzpq2_s32_tied2, svint32_t,
		z0 = svuzpq2_s32 (z1, z0),
		z0 = svuzpq2 (z1, z0))

/*
** uzpq2_s32_untied:
**	uzpq2	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (uzpq2_s32_untied, svint32_t,
		z0 = svuzpq2_s32 (z1, z2),
		z0 = svuzpq2 (z1, z2))
