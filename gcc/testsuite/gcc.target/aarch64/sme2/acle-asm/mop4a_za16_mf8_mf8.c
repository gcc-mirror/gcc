/* { dg-do assemble { target aarch64_asm_sme-mop4_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-mop4_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } }  */

#pragma GCC target "+sve2,+sme-mop4,+sme-f8f16"
#include <arm_sme.h>
#include "test_sme2_acle.h"

/*
** mop4a_1x1_za16_mf8_mf8_0:
**	...
**	fmop4a	za0\.h, z0\.b, z30\.b
**	ret
*/
TEST_UNIFORM_ZA (mop4a_1x1_za16_mf8_mf8_0, svmfloat8_t,
		 svmop4a_1x1_za16_mf8_mf8_fpm (0, z0, z1, fpm0),
		 svmop4a_za16_fpm (0, z0, z1, fpm0));

/*
** mop4a_1x1_za16_mf8_mf8_1:
**	...
**	fmop4a	za1\.h, z0\.b, z30\.b
**	ret
*/
TEST_UNIFORM_ZA (mop4a_1x1_za16_mf8_mf8_1, svmfloat8_t,
		 svmop4a_1x1_za16_mf8_mf8_fpm (1, z0, z1, fpm0),
		 svmop4a_za16_fpm (1, z0, z1, fpm0));

/*
** mop4a_1x2_za16_mf8_mf8_0:
**	...
**	fmop4a	za0\.h, z0\.b, {z30\.b - z31\.b}
**	ret
*/
TEST_DUAL_ZA (mop4a_1x2_za16_mf8_mf8_0, svmfloat8_t, svmfloat8x2_t,
		 svmop4a_1x2_za16_mf8_mf8_fpm (0, z0, z4, fpm0),
		 svmop4a_za16_fpm (0, z0, z4, fpm0));

/*
** mop4a_1x2_za16_mf8_mf8_1:
**	...
**	fmop4a	za1\.h, z0\.b, {z30\.b - z31\.b}
**	ret
*/
TEST_DUAL_ZA (mop4a_1x2_za16_mf8_mf8_1, svmfloat8_t, svmfloat8x2_t,
		 svmop4a_1x2_za16_mf8_mf8_fpm (1, z0, z4, fpm0),
		 svmop4a_za16_fpm (1, z0, z4, fpm0));

/*
** mop4a_2x1_za16_mf8_mf8_0:
**	...
**	fmop4a	za0\.h, {z0\.b - z1\.b}, z30\.b
**	ret
*/
TEST_DUAL_ZA (mop4a_2x1_za16_mf8_mf8_0, svmfloat8x2_t, svmfloat8_t,
		 svmop4a_2x1_za16_mf8_mf8_fpm (0, z0, z4, fpm0),
		 svmop4a_za16_fpm (0, z0, z4, fpm0));

/*
** mop4a_2x1_za16_mf8_mf8_1:
**	...
**	fmop4a	za1\.h, {z0\.b - z1\.b}, z30\.b
**	ret
*/
TEST_DUAL_ZA (mop4a_2x1_za16_mf8_mf8_1, svmfloat8x2_t, svmfloat8_t,
		 svmop4a_2x1_za16_mf8_mf8_fpm (1, z0, z4, fpm0),
		 svmop4a_za16_fpm (1, z0, z4, fpm0));

/*
** mop4a_2x2_za16_mf8_mf8_0:
**	...
**	fmop4a	za0\.h, {z0\.b - z1\.b}, {z30\.b - z31\.b}
**	ret
*/
TEST_UNIFORM_ZA (mop4a_2x2_za16_mf8_mf8_0, svmfloat8x2_t,
		 svmop4a_2x2_za16_mf8_mf8_fpm (0, z0, z1, fpm0),
		 svmop4a_za16_fpm (0, z0, z1, fpm0));

/*
** mop4a_2x2_za16_mf8_mf8_1:
**	...
**	fmop4a	za1\.h, {z0\.b - z1\.b}, {z30\.b - z31\.b}
**	ret
*/
TEST_UNIFORM_ZA (mop4a_2x2_za16_mf8_mf8_1, svmfloat8x2_t,
		 svmop4a_2x2_za16_mf8_mf8_fpm (1, z0, z1, fpm0),
		 svmop4a_za16_fpm (1, z0, z1, fpm0));
