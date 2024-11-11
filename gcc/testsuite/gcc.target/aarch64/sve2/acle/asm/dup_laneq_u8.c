/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** dup_laneq_0_u8_tied1:
**	dupq	z0\.b, z0\.b\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_0_u8_tied1, svuint8_t,
		z0 = svdup_laneq_u8 (z0, 0),
		z0 = svdup_laneq (z0, 0))

/*
** dup_laneq_0_u8_untied:
**	dupq	z0\.b, z1\.b\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_0_u8_untied, svuint8_t,
		z0 = svdup_laneq_u8 (z1, 0),
		z0 = svdup_laneq (z1, 0))

/*
** dup_laneq_7_u8:
**	dupq	z0\.b, z0\.b\[7\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_7_u8, svuint8_t,
		z0 = svdup_laneq_u8 (z0, 7),
		z0 = svdup_laneq (z0, 7))

/*
** dup_laneq_8_u8:
**	dupq	z0\.b, z0\.b\[8\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_8_u8, svuint8_t,
		z0 = svdup_laneq_u8 (z0, 8),
		z0 = svdup_laneq (z0, 8))

/*
** dup_laneq_15_u8:
**	dupq	z0\.b, z0\.b\[15\]
**	ret
*/
TEST_UNIFORM_Z (dup_laneq_15_u8, svuint8_t,
		z0 = svdup_laneq_u8 (z0, 15),
		z0 = svdup_laneq (z0, 15))
