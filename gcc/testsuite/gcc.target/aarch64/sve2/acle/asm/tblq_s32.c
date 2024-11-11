/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** tblq_s32_tied1:
**	tblq	z0\.s, {z0\.s}, z4\.s
**	ret
*/
TEST_DUAL_Z (tblq_s32_tied1, svint32_t, svuint32_t,
	     z0 = svtblq_s32 (z0, z4),
	     z0 = svtblq (z0, z4))

/*
** tblq_s32_tied2:
**	tblq	z0\.s, {z4\.s}, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (tblq_s32_tied2, svint32_t, svuint32_t,
		 z0_res = svtblq_s32 (z4, z0),
		 z0_res = svtblq (z4, z0))

/*
** tblq_s32_untied:
**	tblq	z0\.s, {z1\.s}, z4\.s
**	ret
*/
TEST_DUAL_Z (tblq_s32_untied, svint32_t, svuint32_t,
	     z0 = svtblq_s32 (z1, z4),
	     z0 = svtblq (z1, z4))
