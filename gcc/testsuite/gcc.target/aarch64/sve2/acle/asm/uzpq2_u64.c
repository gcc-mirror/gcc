/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** uzpq2_u64_tied1:
** (
**	uzpq2	z0\.d, z0\.d, z1\.d
** |
**	trn2	z0\.d, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (uzpq2_u64_tied1, svuint64_t,
		z0 = svuzpq2_u64 (z0, z1),
		z0 = svuzpq2 (z0, z1))

/*
** uzpq2_u64_tied2:
** (
**	uzpq2	z0\.d, z1\.d, z0\.d
** |
**	trn2	z0\.d, z1\.d, z0\.d
** )
**	ret
*/
TEST_UNIFORM_Z (uzpq2_u64_tied2, svuint64_t,
		z0 = svuzpq2_u64 (z1, z0),
		z0 = svuzpq2 (z1, z0))

/*
** uzpq2_u64_untied:
** (
**	uzpq2	z0\.d, z1\.d, z2\.d
** |
**	trn2	z0\.d, z1\.d, z2\.d
** )
**	ret
*/
TEST_UNIFORM_Z (uzpq2_u64_untied, svuint64_t,
		z0 = svuzpq2_u64 (z1, z2),
		z0 = svuzpq2 (z1, z2))
