/* { dg-do assemble { target aarch64_asm_sme-mop4_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-mop4_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } }  */

#pragma GCC target "+sve2,+sme-mop4,+sme-b16b16"
#include <arm_sme.h>
#include "test_sme2_acle.h"

/*
** mop4s_1x1_za32_bf16_bf16_0:
**	...
**	bfmop4s	za0\.s, z0\.h, z30\.h
**	ret
*/
TEST_UNIFORM_ZA (mop4s_1x1_za32_bf16_bf16_0, svbfloat16_t,
		 svmop4s_1x1_za32_bf16_bf16 (0, z0, z1),
		 svmop4s_za32 (0, z0, z1));

/*
** mop4s_1x1_za32_bf16_bf16_3:
**	...
**	bfmop4s	za3\.s, z0\.h, z30\.h
**	ret
*/
TEST_UNIFORM_ZA (mop4s_1x1_za32_bf16_bf16_3, svbfloat16_t,
		 svmop4s_1x1_za32_bf16_bf16 (3, z0, z1),
		 svmop4s_za32 (3, z0, z1));

/*
** mop4s_1x2_za32_bf16_bf16_0:
**	...
**	bfmop4s	za0\.s, z0\.h, {z30\.h - z31\.h}
**	ret
*/
TEST_DUAL_ZA (mop4s_1x2_za32_bf16_bf16_0, svbfloat16_t, svbfloat16x2_t,
	      svmop4s_1x2_za32_bf16_bf16 (0, z0, z4),
	      svmop4s_za32 (0, z0, z4));

/*
** mop4s_1x2_za32_bf16_bf16_3:
**	...
**	bfmop4s	za3\.s, z0\.h, {z30\.h - z31\.h}
**	ret
*/
TEST_DUAL_ZA (mop4s_1x2_za32_bf16_bf16_3, svbfloat16_t, svbfloat16x2_t,
	      svmop4s_1x2_za32_bf16_bf16 (3, z0, z4),
	      svmop4s_za32 (3, z0, z4));

/*
** mop4s_2x1_za32_bf16_bf16_0:
**	...
**	bfmop4s	za0\.s, {z0\.h - z1\.h}, z30\.h
**	ret
*/
TEST_DUAL_ZA (mop4s_2x1_za32_bf16_bf16_0, svbfloat16x2_t, svbfloat16_t,
	      svmop4s_2x1_za32_bf16_bf16 (0, z0, z4),
	      svmop4s_za32 (0, z0, z4));

/*
** mop4s_2x1_za32_bf16_bf16_3:
**	...
**	bfmop4s	za3\.s, {z0\.h - z1\.h}, z30\.h
**	ret
*/
TEST_DUAL_ZA (mop4s_2x1_za32_bf16_bf16_3, svbfloat16x2_t, svbfloat16_t,
	      svmop4s_2x1_za32_bf16_bf16 (3, z0, z4),
	      svmop4s_za32 (3, z0, z4));

/*
** mop4s_2x2_za32_bf16_bf16_0:
**	...
**	bfmop4s	za0\.s, {z0\.h - z1\.h}, {z30\.h - z31\.h}
**	ret
*/
TEST_UNIFORM_ZA (mop4s_2x2_za32_bf16_bf16_0, svbfloat16x2_t,
		 svmop4s_2x2_za32_bf16_bf16 (0, z0, z1),
		 svmop4s_za32 (0, z0, z1));

/*
** mop4s_2x2_za32_bf16_bf16_3:
**	...
**	bfmop4s	za3\.s, {z0\.h - z1\.h}, {z30\.h - z31\.h}
**	ret
*/
TEST_UNIFORM_ZA (mop4s_2x2_za32_bf16_bf16_3, svbfloat16x2_t,
		 svmop4s_2x2_za32_bf16_bf16 (3, z0, z1),
		 svmop4s_za32 (3, z0, z1));
