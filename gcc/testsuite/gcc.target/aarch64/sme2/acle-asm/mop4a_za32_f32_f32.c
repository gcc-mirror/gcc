/* { dg-do assemble { target aarch64_asm_sme-mop4_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-mop4_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } }  */

#pragma GCC target "+sve2,+sme-mop4"
#include <arm_sme.h>
#include "test_sme2_acle.h"

/*
** mop4a_1x1_za32_f32_f32_0:
**	...
**	fmop4a	za0\.s, z0\.s, z30\.s
**	ret
*/
TEST_UNIFORM_ZA (mop4a_1x1_za32_f32_f32_0, svfloat32_t,
		 svmop4a_1x1_za32_f32_f32 (0, z0, z1),
		 svmop4a_za32 (0, z0, z1));

/*
** mop4a_1x1_za32_f32_f32_3:
**	...
**	fmop4a	za3\.s, z0\.s, z30\.s
**	ret
*/
TEST_UNIFORM_ZA (mop4a_1x1_za32_f32_f32_3, svfloat32_t,
		 svmop4a_1x1_za32_f32_f32 (3, z0, z1),
		 svmop4a_za32 (3, z0, z1));

/*
** mop4a_1x2_za32_f32_f32_0:
**	...
**	fmop4a	za0\.s, z0\.s, {z30\.s - z31\.s}
**	ret
*/
TEST_DUAL_ZA (mop4a_1x2_za32_f32_f32_0, svfloat32_t, svfloat32x2_t,
	      svmop4a_1x2_za32_f32_f32 (0, z0, z4),
	      svmop4a_za32 (0, z0, z4));

/*
** mop4a_1x2_za32_f32_f32_3:
**	...
**	fmop4a	za3\.s, z0\.s, {z30\.s - z31\.s}
**	ret
*/
TEST_DUAL_ZA (mop4a_1x2_za32_f32_f32_3, svfloat32_t, svfloat32x2_t,
	      svmop4a_1x2_za32_f32_f32 (3, z0, z4),
	      svmop4a_za32 (3, z0, z4));

/*
** mop4a_2x1_za32_f32_f32_0:
**	...
**	fmop4a	za0\.s, {z0\.s - z1\.s}, z30\.s
**	ret
*/
TEST_DUAL_ZA (mop4a_2x1_za32_f32_f32_0, svfloat32x2_t, svfloat32_t,
	      svmop4a_2x1_za32_f32_f32 (0, z0, z4),
	      svmop4a_za32 (0, z0, z4));

/*
** mop4a_2x1_za32_f32_f32_3:
**	...
**	fmop4a	za3\.s, {z0\.s - z1\.s}, z30\.s
**	ret
*/
TEST_DUAL_ZA (mop4a_2x1_za32_f32_f32_3, svfloat32x2_t, svfloat32_t,
	      svmop4a_2x1_za32_f32_f32 (3, z0, z4),
	      svmop4a_za32 (3, z0, z4));

/*
** mop4a_2x2_za32_f32_f32_0:
**	...
**	fmop4a	za0\.s, {z0\.s - z1\.s}, {z30\.s - z31\.s}
**	ret
*/
TEST_UNIFORM_ZA (mop4a_2x2_za32_f32_f32_0, svfloat32x2_t,
		 svmop4a_2x2_za32_f32_f32 (0, z0, z1),
		 svmop4a_za32 (0, z0, z1));

/*
** mop4a_2x2_za32_f32_f32_3:
**	...
**	fmop4a	za3\.s, {z0\.s - z1\.s}, {z30\.s - z31\.s}
**	ret
*/
TEST_UNIFORM_ZA (mop4a_2x2_za32_f32_f32_3, svfloat32x2_t,
		 svmop4a_2x2_za32_f32_f32 (3, z0, z1),
		 svmop4a_za32 (3, z0, z1));
