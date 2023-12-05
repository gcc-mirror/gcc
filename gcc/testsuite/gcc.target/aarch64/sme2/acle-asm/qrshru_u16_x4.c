/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qrshru_z0_z0_1:
**	sqrshru	z0\.h, {z0\.d - z3\.d}, #1
**	ret
*/
TEST_X4_NARROW (qrshru_z0_z0_1, svint64x4_t, svuint16_t,
		z0_res = svqrshru_n_u16_s64_x4 (z0, 1),
		z0_res = svqrshru_u16 (z0, 1))

/*
** qrshru_z0_z4_64:
**	sqrshru	z0\.h, {z4\.d - z7\.d}, #64
**	ret
*/
TEST_X4_NARROW (qrshru_z0_z4_64, svint64x4_t, svuint16_t,
		z0_res = svqrshru_n_u16_s64_x4 (z4, 64),
		z0_res = svqrshru_u16 (z4, 64))

/*
** qrshru_z0_z21_33:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshru	z0\.h, [^\n]+, #33
**	ret
*/
TEST_X4_NARROW (qrshru_z0_z21_33, svint64x4_t, svuint16_t,
		z0_res = svqrshru_n_u16_s64_x4 (z21, 33),
		z0_res = svqrshru_u16 (z21, 33))

/*
** qrshru_z25_z26_12:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshru	z25\.h, [^\n]+, #12
**	ret
*/
TEST_X4_NARROW (qrshru_z25_z26_12, svint64x4_t, svuint16_t,
		z25 = svqrshru_n_u16_s64_x4 (z26, 12),
		z25 = svqrshru_u16 (z26, 12))

/*
** qrshru_z25_z0_32:
**	sqrshru	z25\.h, {z0\.d - z3\.d}, #32
**	ret
*/
TEST_X4_NARROW (qrshru_z25_z0_32, svint64x4_t, svuint16_t,
		z25 = svqrshru_n_u16_s64_x4 (z0, 32),
		z25 = svqrshru_u16 (z0, 32))

/*
** qrshru_z22_z16_63:
**	sqrshru	z22\.h, {z16\.d - z19\.d}, #63
**	ret
*/
TEST_X4_NARROW (qrshru_z22_z16_63, svint64x4_t, svuint16_t,
		z22_res = svqrshru_n_u16_s64_x4 (z16, 63),
		z22_res = svqrshru_u16 (z16, 63))
