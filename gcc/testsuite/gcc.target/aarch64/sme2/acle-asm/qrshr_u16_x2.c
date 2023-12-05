/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qrshr_z0_z0_1:
**	uqrshr	z0\.h, {z0\.s - z1\.s}, #1
**	ret
*/
TEST_X2_NARROW (qrshr_z0_z0_1, svuint32x2_t, svuint16_t,
		z0_res = svqrshr_n_u16_u32_x2 (z0, 1),
		z0_res = svqrshr_u16 (z0, 1))

/*
** qrshr_z0_z6_16:
**	uqrshr	z0\.h, {z6\.s - z7\.s}, #16
**	ret
*/
TEST_X2_NARROW (qrshr_z0_z6_16, svuint32x2_t, svuint16_t,
		z0_res = svqrshr_n_u16_u32_x2 (z6, 16),
		z0_res = svqrshr_u16 (z6, 16))

/*
** qrshr_z0_z29_13:
**	mov	[^\n]+
**	mov	[^\n]+
**	uqrshr	z0\.h, [^\n]+, #13
**	ret
*/
TEST_X2_NARROW (qrshr_z0_z29_13, svuint32x2_t, svuint16_t,
		z0_res = svqrshr_n_u16_u32_x2 (z29, 13),
		z0_res = svqrshr_u16 (z29, 13))

/*
** qrshr_z5_z0_11:
**	uqrshr	z5\.h, {z0\.s - z1\.s}, #11
**	ret
*/
TEST_X2_NARROW (qrshr_z5_z0_11, svuint32x2_t, svuint16_t,
		z5 = svqrshr_n_u16_u32_x2 (z0, 11),
		z5 = svqrshr_u16 (z0, 11))

/*
** qrshr_z22_z16_15:
**	uqrshr	z22\.h, {z16\.s - z17\.s}, #15
**	ret
*/
TEST_X2_NARROW (qrshr_z22_z16_15, svuint32x2_t, svuint16_t,
		z22 = svqrshr_n_u16_u32_x2 (z16, 15),
		z22 = svqrshr_u16 (z16, 15))
