/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** read_za16_s16_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	{z0\.h - z3\.h}, za0h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_s16_z0_0_0, svint16x4_t,
		 z0 = svread_hor_za16_s16_vg4 (0, 0),
		 z0 = svread_hor_za16_s16_vg4 (0, 0))

/*
** read_za16_u16_z4_1_1:
**	mov	(w1[2-5]), #?1
**	mova	{z4\.h - z7\.h}, za1h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z4_1_1, svuint16x4_t,
		 z4 = svread_hor_za16_u16_vg4 (1, 1),
		 z4 = svread_hor_za16_u16_vg4 (1, 1))

/*
** read_za16_f16_z28_0_w11:
**	mov	(w1[2-5]), w11
**	mova	{z28\.h - z31\.h}, za0h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_f16_z28_0_w11, svfloat16x4_t,
		 z28 = svread_hor_za16_f16_vg4 (0, w11),
		 z28 = svread_hor_za16_f16_vg4 (0, w11))

/*
** read_za16_s16_z0_1_w12:
**	mova	{z0\.h - z3\.h}, za1h\.h\[w12, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_s16_z0_1_w12, svint16x4_t,
		 z0 = svread_hor_za16_s16_vg4 (1, w12),
		 z0 = svread_hor_za16_s16_vg4 (1, w12))

/*
** read_za16_u16_z18_0_w15:
**	mova	{[^\n]+}, za0h\.h\[w15, 0:3\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z18_0_w15, svuint16x4_t,
		 z18 = svread_hor_za16_u16_vg4 (0, w15),
		 z18 = svread_hor_za16_u16_vg4 (0, w15))

/*
** read_za16_bf16_z23_1_w12p4:
**	mova	{[^\n]+}, za1h\.h\[w12, 4:7\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_za16_bf16_z23_1_w12p4, svbfloat16x4_t,
		 z23 = svread_hor_za16_bf16_vg4 (1, w12 + 4),
		 z23 = svread_hor_za16_bf16_vg4 (1, w12 + 4))

/*
** read_za16_u16_z4_0_w12p1:
**	add	(w[0-9]+), w12, #?1
**	mova	{z4\.h - z7\.h}, za0h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z4_0_w12p1, svuint16x4_t,
		 z4 = svread_hor_za16_u16_vg4 (0, w12 + 1),
		 z4 = svread_hor_za16_u16_vg4 (0, w12 + 1))

/*
** read_za16_s16_z28_1_w12p2:
**	add	(w[0-9]+), w12, #?2
**	mova	{z28\.h - z31\.h}, za1h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_s16_z28_1_w12p2, svint16x4_t,
		 z28 = svread_hor_za16_s16_vg4 (1, w12 + 2),
		 z28 = svread_hor_za16_s16_vg4 (1, w12 + 2))

/*
** read_za16_f16_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	mova	{z0\.h - z3\.h}, za0h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_f16_z0_0_w15p3, svfloat16x4_t,
		 z0 = svread_hor_za16_f16_vg4 (0, w15 + 3),
		 z0 = svread_hor_za16_f16_vg4 (0, w15 + 3))

/*
** read_za16_u16_z28_1_w12p6:
**	add	(w[0-9]+), w12, #?6
**	mova	{z28\.h - z31\.h}, za1h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z28_1_w12p6, svuint16x4_t,
		 z28 = svread_hor_za16_u16_vg4 (1, w12 + 6),
		 z28 = svread_hor_za16_u16_vg4 (1, w12 + 6))

/*
** read_za16_s16_z0_0_w15p8:
**	add	(w[0-9]+), w15, #?8
**	mova	{z0\.h - z3\.h}, za0h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_s16_z0_0_w15p8, svint16x4_t,
		 z0 = svread_hor_za16_s16_vg4 (0, w15 + 8),
		 z0 = svread_hor_za16_s16_vg4 (0, w15 + 8))

/*
** read_za16_bf16_z4_1_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	mova	{z4\.h - z7\.h}, za1h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_bf16_z4_1_w12m1, svbfloat16x4_t,
		 z4 = svread_hor_za16_bf16_vg4 (1, w12 - 1),
		 z4 = svread_hor_za16_bf16_vg4 (1, w12 - 1))

/*
** read_za16_u16_z28_0_w16:
**	mov	(w1[2-5]), w16
**	mova	{z28\.h - z31\.h}, za0h\.h\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z28_0_w16, svuint16x4_t,
		 z28 = svread_hor_za16_u16_vg4 (0, w16),
		 z28 = svread_hor_za16_u16_vg4 (0, w16))
