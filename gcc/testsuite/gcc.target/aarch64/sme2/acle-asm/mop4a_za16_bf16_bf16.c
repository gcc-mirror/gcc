/* { dg-do assemble { target aarch64_asm_sme-mop4_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-mop4_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } }  */

#pragma GCC target "+sve2,+sme-mop4,+sme-b16b16"
#include <arm_sme.h>
#include "test_sme2_acle.h"

/*
** mop4a_1x1_za16_bf16_bf16_0:
**	...
**	bfmop4a	za0\.h, z0\.h, z30\.h
**	ret
*/
TEST_UNIFORM_ZA (mop4a_1x1_za16_bf16_bf16_0, svbfloat16_t,
		 svmop4a_1x1_za16_bf16_bf16 (0, z0, z1),
		 svmop4a_za16 (0, z0, z1));

/*
** mop4a_1x1_za16_bf16_bf16_1:
**	...
**	bfmop4a	za1\.h, z0\.h, z30\.h
**	ret
*/
TEST_UNIFORM_ZA (mop4a_1x1_za16_bf16_bf16_1, svbfloat16_t,
		 svmop4a_1x1_za16_bf16_bf16 (1, z0, z1),
		 svmop4a_za16 (1, z0, z1));

/*
** mop4a_1x2_za16_bf16_bf16_0:
**	...
**	bfmop4a	za0\.h, z0\.h, {z30\.h - z31\.h}
**	ret
*/
TEST_DUAL_ZA (mop4a_1x2_za16_bf16_bf16_0, svbfloat16_t, svbfloat16x2_t,
	      svmop4a_1x2_za16_bf16_bf16 (0, z0, z4),
	      svmop4a_za16 (0, z0, z4));

/*
** mop4a_1x2_za16_bf16_bf16_1:
**	...
**	bfmop4a	za1\.h, z0\.h, {z30\.h - z31\.h}
**	ret
*/
TEST_DUAL_ZA (mop4a_1x2_za16_bf16_bf16_1, svbfloat16_t, svbfloat16x2_t,
	      svmop4a_1x2_za16_bf16_bf16 (1, z0, z4),
	      svmop4a_za16 (1, z0, z4));

/*
** mop4a_2x1_za16_bf16_bf16_0:
**	...
**	bfmop4a	za0\.h, {z0\.h - z1\.h}, z30\.h
**	ret
*/
TEST_DUAL_ZA (mop4a_2x1_za16_bf16_bf16_0, svbfloat16x2_t, svbfloat16_t,
	      svmop4a_2x1_za16_bf16_bf16 (0, z0, z4),
	      svmop4a_za16 (0, z0, z4));

/*
** mop4a_2x1_za16_bf16_bf16_1:
**	...
**	bfmop4a	za1\.h, {z0\.h - z1\.h}, z30\.h
**	ret
*/
TEST_DUAL_ZA (mop4a_2x1_za16_bf16_bf16_1, svbfloat16x2_t, svbfloat16_t,
	      svmop4a_2x1_za16_bf16_bf16 (1, z0, z4),
	      svmop4a_za16 (1, z0, z4));

/*
** mop4a_2x2_za16_bf16_bf16_0:
**	...
**	bfmop4a	za0\.h, {z0\.h - z1\.h}, {z30\.h - z31\.h}
**	ret
*/
TEST_UNIFORM_ZA (mop4a_2x2_za16_bf16_bf16_0, svbfloat16x2_t,
		 svmop4a_2x2_za16_bf16_bf16 (0, z0, z1),
		 svmop4a_za16 (0, z0, z1));

/*
** mop4a_2x2_za16_bf16_bf16_1:
**	...
**	bfmop4a	za1\.h, {z0\.h - z1\.h}, {z30\.h - z31\.h}
**	ret
*/
TEST_UNIFORM_ZA (mop4a_2x2_za16_bf16_bf16_1, svbfloat16x2_t,
		 svmop4a_2x2_za16_bf16_bf16 (1, z0, z1),
		 svmop4a_za16 (1, z0, z1));
