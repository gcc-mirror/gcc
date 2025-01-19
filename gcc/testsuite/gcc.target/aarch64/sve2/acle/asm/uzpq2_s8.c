/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** uzpq2_s8_tied1:
**	uzpq2	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (uzpq2_s8_tied1, svint8_t,
		z0 = svuzpq2_s8 (z0, z1),
		z0 = svuzpq2 (z0, z1))

/*
** uzpq2_s8_tied2:
**	uzpq2	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (uzpq2_s8_tied2, svint8_t,
		z0 = svuzpq2_s8 (z1, z0),
		z0 = svuzpq2 (z1, z0))

/*
** uzpq2_s8_untied:
**	uzpq2	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (uzpq2_s8_untied, svint8_t,
		z0 = svuzpq2_s8 (z1, z2),
		z0 = svuzpq2 (z1, z2))
