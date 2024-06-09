/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qrshru_z0_z0_1:
**	sqrshru	z0\.h, {z0\.s - z1\.s}, #1
**	ret
*/
TEST_X2_NARROW (qrshru_z0_z0_1, svint32x2_t, svuint16_t,
		z0_res = svqrshru_n_u16_s32_x2 (z0, 1),
		z0_res = svqrshru_u16 (z0, 1))

/*
** qrshru_z0_z6_16:
**	sqrshru	z0\.h, {z6\.s - z7\.s}, #16
**	ret
*/
TEST_X2_NARROW (qrshru_z0_z6_16, svint32x2_t, svuint16_t,
		z0_res = svqrshru_n_u16_s32_x2 (z6, 16),
		z0_res = svqrshru_u16 (z6, 16))

/*
** qrshru_z0_z29_13:
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshru	z0\.h, [^\n]+, #13
**	ret
*/
TEST_X2_NARROW (qrshru_z0_z29_13, svint32x2_t, svuint16_t,
		z0_res = svqrshru_n_u16_s32_x2 (z29, 13),
		z0_res = svqrshru_u16 (z29, 13))

/*
** qrshru_z5_z0_11:
**	sqrshru	z5\.h, {z0\.s - z1\.s}, #11
**	ret
*/
TEST_X2_NARROW (qrshru_z5_z0_11, svint32x2_t, svuint16_t,
		z5 = svqrshru_n_u16_s32_x2 (z0, 11),
		z5 = svqrshru_u16 (z0, 11))

/*
** qrshru_z22_z16_15:
**	sqrshru	z22\.h, {z16\.s - z17\.s}, #15
**	ret
*/
TEST_X2_NARROW (qrshru_z22_z16_15, svint32x2_t, svuint16_t,
		z22 = svqrshru_n_u16_s32_x2 (z16, 15),
		z22 = svqrshru_u16 (z16, 15))
