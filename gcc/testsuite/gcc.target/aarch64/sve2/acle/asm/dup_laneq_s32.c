/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** dup_laneq_0_s32_tied1:
**	dupq	z0\.s, z0\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_0_s32_tied1, svint32_t,
		z0 = svdup_laneq_s32 (z0, 0),
		z0 = svdup_laneq (z0, 0))

/*
** dup_laneq_0_s32_untied:
**	dupq	z0\.s, z1\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_0_s32_untied, svint32_t,
		z0 = svdup_laneq_s32 (z1, 0),
		z0 = svdup_laneq (z1, 0))

/*
** dup_laneq_1_s32:
**	dupq	z0\.s, z0\.s\[1\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_1_s32, svint32_t,
		z0 = svdup_laneq_s32 (z0, 1),
		z0 = svdup_laneq (z0, 1))

/*
** dup_laneq_2_s32:
**	dupq	z0\.s, z0\.s\[2\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_2_s32, svint32_t,
		z0 = svdup_laneq_s32 (z0, 2),
		z0 = svdup_laneq (z0, 2))

/*
** dup_laneq_3_s32:
**	dupq	z0\.s, z0\.s\[3\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_3_s32, svint32_t,
		z0 = svdup_laneq_s32 (z0, 3),
		z0 = svdup_laneq (z0, 3))
