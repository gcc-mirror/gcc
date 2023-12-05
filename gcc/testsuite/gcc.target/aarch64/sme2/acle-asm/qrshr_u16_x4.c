/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qrshr_z0_z0_1:
**	uqrshr	z0\.h, {z0\.d - z3\.d}, #1
**	ret
*/
TEST_X4_NARROW (qrshr_z0_z0_1, svuint64x4_t, svuint16_t,
		z0_res = svqrshr_n_u16_u64_x4 (z0, 1),
		z0_res = svqrshr_u16 (z0, 1))

/*
** qrshr_z0_z4_64:
**	uqrshr	z0\.h, {z4\.d - z7\.d}, #64
**	ret
*/
TEST_X4_NARROW (qrshr_z0_z4_64, svuint64x4_t, svuint16_t,
		z0_res = svqrshr_n_u16_u64_x4 (z4, 64),
		z0_res = svqrshr_u16 (z4, 64))

/*
** qrshr_z0_z21_33:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uqrshr	z0\.h, [^\n]+, #33
**	ret
*/
TEST_X4_NARROW (qrshr_z0_z21_33, svuint64x4_t, svuint16_t,
		z0_res = svqrshr_n_u16_u64_x4 (z21, 33),
		z0_res = svqrshr_u16 (z21, 33))

/*
** qrshr_z25_z26_12:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uqrshr	z25\.h, [^\n]+, #12
**	ret
*/
TEST_X4_NARROW (qrshr_z25_z26_12, svuint64x4_t, svuint16_t,
		z25 = svqrshr_n_u16_u64_x4 (z26, 12),
		z25 = svqrshr_u16 (z26, 12))

/*
** qrshr_z25_z0_32:
**	uqrshr	z25\.h, {z0\.d - z3\.d}, #32
**	ret
*/
TEST_X4_NARROW (qrshr_z25_z0_32, svuint64x4_t, svuint16_t,
		z25 = svqrshr_n_u16_u64_x4 (z0, 32),
		z25 = svqrshr_u16 (z0, 32))

/*
** qrshr_z22_z16_63:
**	uqrshr	z22\.h, {z16\.d - z19\.d}, #63
**	ret
*/
TEST_X4_NARROW (qrshr_z22_z16_63, svuint64x4_t, svuint16_t,
		z22_res = svqrshr_n_u16_u64_x4 (z16, 63),
		z22_res = svqrshr_u16 (z16, 63))
