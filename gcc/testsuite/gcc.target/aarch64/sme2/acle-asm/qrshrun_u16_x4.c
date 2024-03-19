/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qrshrun_z0_z0_1:
**	sqrshrun	z0\.h, {z0\.d - z3\.d}, #1
**	ret
*/
TEST_X4_NARROW (qrshrun_z0_z0_1, svint64x4_t, svuint16_t,
		z0_res = svqrshrun_n_u16_s64_x4 (z0, 1),
		z0_res = svqrshrun_u16 (z0, 1))

/*
** qrshrun_z0_z4_64:
**	sqrshrun	z0\.h, {z4\.d - z7\.d}, #64
**	ret
*/
TEST_X4_NARROW (qrshrun_z0_z4_64, svint64x4_t, svuint16_t,
		z0_res = svqrshrun_n_u16_s64_x4 (z4, 64),
		z0_res = svqrshrun_u16 (z4, 64))

/*
** qrshrun_z0_z21_33:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshrun	z0\.h, [^\n]+, #33
**	ret
*/
TEST_X4_NARROW (qrshrun_z0_z21_33, svint64x4_t, svuint16_t,
		z0_res = svqrshrun_n_u16_s64_x4 (z21, 33),
		z0_res = svqrshrun_u16 (z21, 33))

/*
** qrshrun_z25_z26_12:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshrun	z25\.h, [^\n]+, #12
**	ret
*/
TEST_X4_NARROW (qrshrun_z25_z26_12, svint64x4_t, svuint16_t,
		z25 = svqrshrun_n_u16_s64_x4 (z26, 12),
		z25 = svqrshrun_u16 (z26, 12))

/*
** qrshrun_z25_z0_32:
**	sqrshrun	z25\.h, {z0\.d - z3\.d}, #32
**	ret
*/
TEST_X4_NARROW (qrshrun_z25_z0_32, svint64x4_t, svuint16_t,
		z25 = svqrshrun_n_u16_s64_x4 (z0, 32),
		z25 = svqrshrun_u16 (z0, 32))

/*
** qrshrun_z22_z16_63:
**	sqrshrun	z22\.h, {z16\.d - z19\.d}, #63
**	ret
*/
TEST_X4_NARROW (qrshrun_z22_z16_63, svint64x4_t, svuint16_t,
		z22_res = svqrshrun_n_u16_s64_x4 (z16, 63),
		z22_res = svqrshrun_u16 (z16, 63))
