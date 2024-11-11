/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** dup_laneq_0_bf16_tied1:
**	dupq	z0\.h, z0\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_0_bf16_tied1, svbfloat16_t,
		z0 = svdup_laneq_bf16 (z0, 0),
		z0 = svdup_laneq (z0, 0))

/*
** dup_laneq_0_bf16_untied:
**	dupq	z0\.h, z1\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_0_bf16_untied, svbfloat16_t,
		z0 = svdup_laneq_bf16 (z1, 0),
		z0 = svdup_laneq (z1, 0))

/*
** dup_laneq_3_bf16:
**	dupq	z0\.h, z0\.h\[3\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_3_bf16, svbfloat16_t,
		z0 = svdup_laneq_bf16 (z0, 3),
		z0 = svdup_laneq (z0, 3))

/*
** dup_laneq_4_bf16:
**	dupq	z0\.h, z0\.h\[4\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_4_bf16, svbfloat16_t,
		z0 = svdup_laneq_bf16 (z0, 4),
		z0 = svdup_laneq (z0, 4))

/*
** dup_laneq_7_bf16:
**	dupq	z0\.h, z0\.h\[7\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_7_bf16, svbfloat16_t,
		z0 = svdup_laneq_bf16 (z0, 7),
		z0 = svdup_laneq (z0, 7))
