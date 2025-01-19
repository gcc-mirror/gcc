/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** zipq2_s8_tied1:
**	zipq2	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (zipq2_s8_tied1, svint8_t,
		z0 = svzipq2_s8 (z0, z1),
		z0 = svzipq2 (z0, z1))

/*
** zipq2_s8_tied2:
**	zipq2	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (zipq2_s8_tied2, svint8_t,
		z0 = svzipq2_s8 (z1, z0),
		z0 = svzipq2 (z1, z0))

/*
** zipq2_s8_untied:
**	zipq2	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (zipq2_s8_untied, svint8_t,
		z0 = svzipq2_s8 (z1, z2),
		z0 = svzipq2 (z1, z2))
