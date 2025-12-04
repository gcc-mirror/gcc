/* { dg-do assemble { target aarch64_asm_sme-mop4_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-mop4_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2,+sme-mop4,+sme-f64f64"
#include <arm_sme.h>
#include "test_sme2_acle.h"

/*
** mop4s_1x1_za64_f64_f64_0:
**	...
**	fmop4s	za0\.d, z0\.d, z30\.d
**	ret
*/
TEST_UNIFORM_ZA (mop4s_1x1_za64_f64_f64_0, svfloat64_t,
		 svmop4s_1x1_za64_f64_f64 (0, z0, z1),
		 svmop4s_za64 (0, z0, z1));

/*
** mop4s_1x1_za64_f64_f64_7:
**	...
**	fmop4s	za7\.d, z0\.d, z30\.d
**	ret
*/
TEST_UNIFORM_ZA (mop4s_1x1_za64_f64_f64_7, svfloat64_t,
		 svmop4s_1x1_za64_f64_f64 (7, z0, z1),
		 svmop4s_za64 (7, z0, z1));

/*
** mop4s_1x2_za64_f64_f64_0:
**	...
**	fmop4s	za0\.d, z0\.d, {z30\.d - z31\.d}
**	ret
*/
TEST_DUAL_ZA (mop4s_1x2_za64_f64_f64_0, svfloat64_t, svfloat64x2_t,
	      svmop4s_1x2_za64_f64_f64 (0, z0, z4),
	      svmop4s_za64 (0, z0, z4));

/*
** mop4s_1x2_za64_f64_f64_7:
**	...
**	fmop4s	za7\.d, z0\.d, {z30\.d - z31\.d}
**	ret
*/
TEST_DUAL_ZA (mop4s_1x2_za64_f64_f64_7, svfloat64_t, svfloat64x2_t,
	      svmop4s_1x2_za64_f64_f64 (7, z0, z4),
	      svmop4s_za64 (7, z0, z4));

/*
** mop4s_2x1_za64_f64_f64_0:
**	...
**	fmop4s	za0\.d, {z0\.d - z1\.d}, z30\.d
**	ret
*/
TEST_DUAL_ZA (mop4s_2x1_za64_f64_f64_0, svfloat64x2_t, svfloat64_t,
	      svmop4s_2x1_za64_f64_f64 (0, z0, z4),
	      svmop4s_za64 (0, z0, z4));

/*
** mop4s_2x1_za64_f64_f64_7:
**	...
**	fmop4s	za7\.d, {z0\.d - z1\.d}, z30\.d
**	ret
*/
TEST_DUAL_ZA (mop4s_2x1_za64_f64_f64_7, svfloat64x2_t, svfloat64_t,
	      svmop4s_2x1_za64_f64_f64 (7, z0, z4),
	      svmop4s_za64 (7, z0, z4));

/*
** mop4s_2x2_za64_f64_f64_0:
**	...
**	fmop4s	za0\.d, {z0\.d - z1\.d}, {z30\.d - z31\.d}
**	ret
*/
TEST_UNIFORM_ZA (mop4s_2x2_za64_f64_f64_0, svfloat64x2_t,
		 svmop4s_2x2_za64_f64_f64 (0, z0, z1),
		 svmop4s_za64 (0, z0, z1));

/*
** mop4s_2x2_za64_f64_f64_7:
**	...
**	fmop4s	za7\.d, {z0\.d - z1\.d}, {z30\.d - z31\.d}
**	ret
*/
TEST_UNIFORM_ZA (mop4s_2x2_za64_f64_f64_7, svfloat64x2_t,
		 svmop4s_2x2_za64_f64_f64 (7, z0, z1),
		 svmop4s_za64 (7, z0, z1));
