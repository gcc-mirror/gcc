/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qrshr_z0_z0_1:
**	uqrshr	z0\.b, {z0\.s - z3\.s}, #1
**	ret
*/
TEST_X4_NARROW (qrshr_z0_z0_1, svuint32x4_t, svuint8_t,
		z0_res = svqrshr_n_u8_u32_x4 (z0, 1),
		z0_res = svqrshr_u8 (z0, 1))

/*
** qrshr_z0_z4_32:
**	uqrshr	z0\.b, {z4\.s - z7\.s}, #32
**	ret
*/
TEST_X4_NARROW (qrshr_z0_z4_32, svuint32x4_t, svuint8_t,
		z0_res = svqrshr_n_u8_u32_x4 (z4, 32),
		z0_res = svqrshr_u8 (z4, 32))

/*
** qrshr_z0_z21_2:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uqrshr	z0\.b, [^\n]+, #2
**	ret
*/
TEST_X4_NARROW (qrshr_z0_z21_2, svuint32x4_t, svuint8_t,
		z0_res = svqrshr_n_u8_u32_x4 (z21, 2),
		z0_res = svqrshr_u8 (z21, 2))

/*
** qrshr_z25_z26_12:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uqrshr	z25\.b, [^\n]+, #12
**	ret
*/
TEST_X4_NARROW (qrshr_z25_z26_12, svuint32x4_t, svuint8_t,
		z25 = svqrshr_n_u8_u32_x4 (z26, 12),
		z25 = svqrshr_u8 (z26, 12))

/*
** qrshr_z25_z0_16:
**	uqrshr	z25\.b, {z0\.s - z3\.s}, #16
**	ret
*/
TEST_X4_NARROW (qrshr_z25_z0_16, svuint32x4_t, svuint8_t,
		z25 = svqrshr_n_u8_u32_x4 (z0, 16),
		z25 = svqrshr_u8 (z0, 16))

/*
** qrshr_z22_z16_31:
**	uqrshr	z22\.b, {z16\.s - z19\.s}, #31
**	ret
*/
TEST_X4_NARROW (qrshr_z22_z16_31, svuint32x4_t, svuint8_t,
		z22_res = svqrshr_n_u8_u32_x4 (z16, 31),
		z22_res = svqrshr_u8 (z16, 31))
