/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** addqv_d0_s64_tied:
**	addqv	v0\.2d, p0, z0\.d
**	ret
*/
TEST_REDUCTION_D (addqv_d0_s64_tied, int64x2_t, svint64_t,
		  d0 = svaddqv_s64 (p0, z0),
		  d0 = svaddqv (p0, z0))

/*
** addqv_d0_s64_untied:
**	addqv	v0\.2d, p0, z1\.d
**	ret
*/
TEST_REDUCTION_D (addqv_d0_s64_untied, int64x2_t, svint64_t,
		  d0 = svaddqv_s64 (p0, z1),
		  d0 = svaddqv (p0, z1))
