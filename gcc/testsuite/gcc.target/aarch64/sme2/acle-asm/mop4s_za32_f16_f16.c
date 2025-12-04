/* { dg-do assemble { target aarch64_asm_sme-mop4_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-mop4_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } }  */

#pragma GCC target "+sve2,+sme-mop4,+sme-f16f16"
#include <arm_sme.h>
#include "test_sme2_acle.h"

/*
** mop4s_1x1_za32_f16_f16_0:
**	...
**	fmop4s	za0\.s, z0\.h, z30\.h
**	ret
*/
TEST_UNIFORM_ZA (mop4s_1x1_za32_f16_f16_0, svfloat16_t,
		 svmop4s_1x1_za32_f16_f16 (0, z0, z1),
		 svmop4s_za32 (0, z0, z1));

/*
** mop4s_1x1_za32_f16_f16_3:
**	...
**	fmop4s	za3\.s, z0\.h, z30\.h
**	ret
*/
TEST_UNIFORM_ZA (mop4s_1x1_za32_f16_f16_3, svfloat16_t,
		 svmop4s_1x1_za32_f16_f16 (3, z0, z1),
		 svmop4s_za32 (3, z0, z1));

/*
** mop4s_1x2_za32_f16_f16_0:
**	...
**	fmop4s	za0\.s, z0\.h, {z30\.h - z31\.h}
**	ret
*/
TEST_DUAL_ZA (mop4s_1x2_za32_f16_f16_0, svfloat16_t, svfloat16x2_t,
	      svmop4s_1x2_za32_f16_f16 (0, z0, z4),
	      svmop4s_za32 (0, z0, z4));

/*
** mop4s_1x2_za32_f16_f16_3:
**	...
**	fmop4s	za3\.s, z0\.h, {z30\.h - z31\.h}
**	ret
*/
TEST_DUAL_ZA (mop4s_1x2_za32_f16_f16_3, svfloat16_t, svfloat16x2_t,
	      svmop4s_1x2_za32_f16_f16 (3, z0, z4),
	      svmop4s_za32 (3, z0, z4));

/*
** mop4s_2x1_za32_f16_f16_0:
**	...
**	fmop4s	za0\.s, {z0\.h - z1\.h}, z30\.h
**	ret
*/
TEST_DUAL_ZA (mop4s_2x1_za32_f16_f16_0, svfloat16x2_t, svfloat16_t,
	      svmop4s_2x1_za32_f16_f16 (0, z0, z4),
	      svmop4s_za32 (0, z0, z4));

/*
** mop4s_2x1_za32_f16_f16_3:
**	...
**	fmop4s	za3\.s, {z0\.h - z1\.h}, z30\.h
**	ret
*/
TEST_DUAL_ZA (mop4s_2x1_za32_f16_f16_3, svfloat16x2_t, svfloat16_t,
	      svmop4s_2x1_za32_f16_f16 (3, z0, z4),
	      svmop4s_za32 (3, z0, z4));

/*
** mop4s_2x2_za32_f16_f16_0:
**	...
**	fmop4s	za0\.s, {z0\.h - z1\.h}, {z30\.h - z31\.h}
**	ret
*/
TEST_UNIFORM_ZA (mop4s_2x2_za32_f16_f16_0, svfloat16x2_t,
		 svmop4s_2x2_za32_f16_f16 (0, z0, z1),
		 svmop4s_za32 (0, z0, z1));

/*
** mop4s_2x2_za32_f16_f16_3:
**	...
**	fmop4s	za3\.s, {z0\.h - z1\.h}, {z30\.h - z31\.h}
**	ret
*/
TEST_UNIFORM_ZA (mop4s_2x2_za32_f16_f16_3, svfloat16x2_t,
		 svmop4s_2x2_za32_f16_f16 (3, z0, z1),
		 svmop4s_za32 (3, z0, z1));
