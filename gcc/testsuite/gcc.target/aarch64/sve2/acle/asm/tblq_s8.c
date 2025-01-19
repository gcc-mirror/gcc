/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** tblq_s8_tied1:
**	tblq	z0\.b, {z0\.b}, z4\.b
**	ret
*/
TEST_DUAL_Z (tblq_s8_tied1, svint8_t, svuint8_t,
	     z0 = svtblq_s8 (z0, z4),
	     z0 = svtblq (z0, z4))

/*
** tblq_s8_tied2:
**	tblq	z0\.b, {z4\.b}, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (tblq_s8_tied2, svint8_t, svuint8_t,
		 z0_res = svtblq_s8 (z4, z0),
		 z0_res = svtblq (z4, z0))

/*
** tblq_s8_untied:
**	tblq	z0\.b, {z1\.b}, z4\.b
**	ret
*/
TEST_DUAL_Z (tblq_s8_untied, svint8_t, svuint8_t,
	     z0 = svtblq_s8 (z1, z4),
	     z0 = svtblq (z1, z4))
