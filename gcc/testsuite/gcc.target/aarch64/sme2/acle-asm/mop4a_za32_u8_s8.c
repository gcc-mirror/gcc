/* { dg-do assemble { target aarch64_asm_sme-mop4_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-mop4_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } }  */

#pragma GCC target "+sve2,+sme-mop4"
#include <arm_sme.h>
#include "test_sme2_acle.h"

/*
** mop4a_1x1_za32_u8_s8_0:
**	...
**	usmop4a	za0\.s, z0\.b, z30\.b
**	ret
*/
TEST_DUAL_ZA (mop4a_1x1_za32_u8_s8_0, svuint8_t, svint8_t,
	      svmop4a_1x1_za32_u8_s8 (0, z0, z4),
	      svmop4a_za32 (0, z0, z4));

/*
** mop4a_1x1_za32_u8_s8_3:
**	...
**	usmop4a	za3\.s, z0\.b, z30\.b
**	ret
*/
TEST_DUAL_ZA (mop4a_1x1_za32_u8_s8_3, svuint8_t, svint8_t,
	      svmop4a_1x1_za32_u8_s8 (3, z0, z4),
	      svmop4a_za32 (3, z0, z4));

/*
** mop4a_1x2_za32_u8_s8_0:
**	...
**	usmop4a	za0\.s, z0\.b, {z30\.b - z31\.b}
**	ret
*/
TEST_DUAL_ZA (mop4a_1x2_za32_u8_s8_0, svuint8_t, svint8x2_t,
	      svmop4a_1x2_za32_u8_s8 (0, z0, z4),
	      svmop4a_za32 (0, z0, z4));

/*
** mop4a_1x2_za32_u8_s8_3:
**	...
**	usmop4a	za3\.s, z0\.b, {z30\.b - z31\.b}
**	ret
*/
TEST_DUAL_ZA (mop4a_1x2_za32_u8_s8_3, svuint8_t, svint8x2_t,
	      svmop4a_1x2_za32_u8_s8 (3, z0, z4),
	      svmop4a_za32 (3, z0, z4));

/*
** mop4a_2x1_za32_u8_s8_0:
**	...
**	usmop4a	za0\.s, {z0\.b - z1\.b}, z30\.b
**	ret
*/
TEST_DUAL_ZA (mop4a_2x1_za32_u8_s8_0, svuint8x2_t, svint8_t,
	      svmop4a_2x1_za32_u8_s8 (0, z0, z4),
	      svmop4a_za32 (0, z0, z4));

/*
** mop4a_2x1_za32_u8_s8_3:
**	...
**	usmop4a	za3\.s, {z0\.b - z1\.b}, z30\.b
**	ret
*/
TEST_DUAL_ZA (mop4a_2x1_za32_u8_s8_3, svuint8x2_t, svint8_t,
	      svmop4a_2x1_za32_u8_s8 (3, z0, z4),
	      svmop4a_za32 (3, z0, z4));

/*
** mop4a_2x2_za32_u8_s8_0:
**	...
**	usmop4a	za0\.s, {z0\.b - z1\.b}, {z30\.b - z31\.b}
**	ret
*/
TEST_DUAL_ZA (mop4a_2x2_za32_u8_s8_0, svuint8x2_t, svint8x2_t,
	      svmop4a_2x2_za32_u8_s8 (0, z0, z4),
	      svmop4a_za32 (0, z0, z4));

/*
** mop4a_2x2_za32_u8_s8_3:
**	...
**	usmop4a	za3\.s, {z0\.b - z1\.b}, {z30\.b - z31\.b}
**	ret
*/
TEST_DUAL_ZA (mop4a_2x2_za32_u8_s8_3, svuint8x2_t, svint8x2_t,
	      svmop4a_2x2_za32_u8_s8 (3, z0, z4),
	      svmop4a_za32 (3, z0, z4));
