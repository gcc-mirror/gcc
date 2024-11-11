/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** zipq2_s64_tied1:
** (
**	zipq2	z0\.d, z0\.d, z1\.d
** |
**	trn2	z0\.d, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (zipq2_s64_tied1, svint64_t,
		z0 = svzipq2_s64 (z0, z1),
		z0 = svzipq2 (z0, z1))

/*
** zipq2_s64_tied2:
** (
**	zipq2	z0\.d, z1\.d, z0\.d
** |
**	trn2	z0\.d, z1\.d, z0\.d
** )
**	ret
*/
TEST_UNIFORM_Z (zipq2_s64_tied2, svint64_t,
		z0 = svzipq2_s64 (z1, z0),
		z0 = svzipq2 (z1, z0))

/*
** zipq2_s64_untied:
** (
**	zipq2	z0\.d, z1\.d, z2\.d
** |
**	trn2	z0\.d, z1\.d, z2\.d
** )
**	ret
*/
TEST_UNIFORM_Z (zipq2_s64_untied, svint64_t,
		z0 = svzipq2_s64 (z1, z2),
		z0 = svzipq2 (z1, z2))
