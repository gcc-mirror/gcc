/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** zipq1_u64_tied1:
** (
**	zipq1	z0\.d, z0\.d, z1\.d
** |
**	trn1	z0\.d, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (zipq1_u64_tied1, svuint64_t,
		z0 = svzipq1_u64 (z0, z1),
		z0 = svzipq1 (z0, z1))

/*
** zipq1_u64_tied2:
** (
**	zipq1	z0\.d, z1\.d, z0\.d
** |
**	trn1	z0\.d, z1\.d, z0\.d
** )
**	ret
*/
TEST_UNIFORM_Z (zipq1_u64_tied2, svuint64_t,
		z0 = svzipq1_u64 (z1, z0),
		z0 = svzipq1 (z1, z0))

/*
** zipq1_u64_untied:
** (
**	zipq1	z0\.d, z1\.d, z2\.d
** |
**	trn1	z0\.d, z1\.d, z2\.d
** )
**	ret
*/
TEST_UNIFORM_Z (zipq1_u64_untied, svuint64_t,
		z0 = svzipq1_u64 (z1, z2),
		z0 = svzipq1 (z1, z2))
