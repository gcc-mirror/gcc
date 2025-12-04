/* { dg-do assemble { target aarch64_asm_sme-mop4_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-mop4_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sve2,+sme-mop4,+sme-i16i64"
#include <arm_sme.h>
#include "test_sme2_acle.h"

/*
** mop4s_1x1_za64_s16_s16_0:
**	...
**	smop4s	za0\.d, z0\.h, z30\.h
**	ret
*/
TEST_UNIFORM_ZA (mop4s_1x1_za64_s16_s16_0, svint16_t,
		 svmop4s_1x1_za64_s16_s16 (0, z0, z1),
		 svmop4s_za64 (0, z0, z1));

/*
** mop4s_1x1_za64_s16_s16_7:
**	...
**	smop4s	za7\.d, z0\.h, z30\.h
**	ret
*/
TEST_UNIFORM_ZA (mop4s_1x1_za64_s16_s16_7, svint16_t,
		 svmop4s_1x1_za64_s16_s16 (7, z0, z1),
		 svmop4s_za64 (7, z0, z1));

/*
** mop4s_1x2_za64_s16_s16_0:
**	...
**	smop4s	za0\.d, z0\.h, {z30\.h - z31\.h}
**	ret
*/
TEST_DUAL_ZA (mop4s_1x2_za64_s16_s16_0, svint16_t, svint16x2_t,
	      svmop4s_1x2_za64_s16_s16 (0, z0, z4),
	      svmop4s_za64 (0, z0, z4));

/*
** mop4s_1x2_za64_s16_s16_7:
**	...
**	smop4s	za7\.d, z0\.h, {z30\.h - z31\.h}
**	ret
*/
TEST_DUAL_ZA (mop4s_1x2_za64_s16_s16_7, svint16_t, svint16x2_t,
	      svmop4s_1x2_za64_s16_s16 (7, z0, z4),
	      svmop4s_za64 (7, z0, z4));

/*
** mop4s_2x1_za64_s16_s16_0:
**	...
**	smop4s	za0\.d, {z0\.h - z1\.h}, z30\.h
**	ret
*/
TEST_DUAL_ZA (mop4s_2x1_za64_s16_s16_0, svint16x2_t, svint16_t,
	      svmop4s_2x1_za64_s16_s16 (0, z0, z4),
	      svmop4s_za64 (0, z0, z4));

/*
** mop4s_2x1_za64_s16_s16_7:
**	...
**	smop4s	za7\.d, {z0\.h - z1\.h}, z30\.h
**	ret
*/
TEST_DUAL_ZA (mop4s_2x1_za64_s16_s16_7, svint16x2_t, svint16_t,
	      svmop4s_2x1_za64_s16_s16 (7, z0, z4),
	      svmop4s_za64 (7, z0, z4));

/*
** mop4s_2x2_za64_s16_s16_0:
**	...
**	smop4s	za0\.d, {z0\.h - z1\.h}, {z30\.h - z31\.h}
**	ret
*/
TEST_UNIFORM_ZA (mop4s_2x2_za64_s16_s16_0, svint16x2_t,
		 svmop4s_2x2_za64_s16_s16 (0, z0, z1),
		 svmop4s_za64 (0, z0, z1));

/*
** mop4s_2x2_za64_s16_s16_7:
**	...
**	smop4s	za7\.d, {z0\.h - z1\.h}, {z30\.h - z31\.h}
**	ret
*/
TEST_UNIFORM_ZA (mop4s_2x2_za64_s16_s16_7, svint16x2_t,
		 svmop4s_2x2_za64_s16_s16 (7, z0, z1),
		 svmop4s_za64 (7, z0, z1));
