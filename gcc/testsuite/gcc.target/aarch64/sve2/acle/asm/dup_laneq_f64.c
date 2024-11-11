/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** dup_laneq_0_f64_tied1:
**	dupq	z0\.d, z0\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_0_f64_tied1, svfloat64_t,
		z0 = svdup_laneq_f64 (z0, 0),
		z0 = svdup_laneq (z0, 0))

/*
** dup_laneq_0_f64_untied:
**	dupq	z0\.d, z1\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_0_f64_untied, svfloat64_t,
		z0 = svdup_laneq_f64 (z1, 0),
		z0 = svdup_laneq (z1, 0))

/*
** dup_laneq_1_f64:
**	dupq	z0\.d, z0\.d\[1\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_1_f64, svfloat64_t,
		z0 = svdup_laneq_f64 (z0, 1),
		z0 = svdup_laneq (z0, 1))
