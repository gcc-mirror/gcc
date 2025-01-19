/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** tblq_f64_tied1:
**	tblq	z0\.d, {z0\.d}, z4\.d
**	ret
*/
TEST_DUAL_Z (tblq_f64_tied1, svfloat64_t, svuint64_t,
	     z0 = svtblq_f64 (z0, z4),
	     z0 = svtblq (z0, z4))

/*
** tblq_f64_tied2:
**	tblq	z0\.d, {z4\.d}, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (tblq_f64_tied2, svfloat64_t, svuint64_t,
		 z0_res = svtblq_f64 (z4, z0),
		 z0_res = svtblq (z4, z0))

/*
** tblq_f64_untied:
**	tblq	z0\.d, {z1\.d}, z4\.d
**	ret
*/
TEST_DUAL_Z (tblq_f64_untied, svfloat64_t, svuint64_t,
	     z0 = svtblq_f64 (z1, z4),
	     z0 = svtblq (z1, z4))
