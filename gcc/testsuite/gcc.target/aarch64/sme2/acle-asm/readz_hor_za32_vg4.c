/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_za32_s32_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	movaz	{z0\.s - z3\.s}, za0h\.s\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_s32_z0_0_0, svint32x4_t,
		 z0 = svreadz_hor_za32_s32_vg4 (0, 0),
		 z0 = svreadz_hor_za32_s32_vg4 (0, 0))

/*
** readz_za32_u32_z4_1_1:
**	mov	(w1[2-5]), #?1
**	movaz	{z4\.s - z7\.s}, za1h\.s\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z4_1_1, svuint32x4_t,
		 z4 = svreadz_hor_za32_u32_vg4 (1, 1),
		 z4 = svreadz_hor_za32_u32_vg4 (1, 1))

/*
** readz_za32_f32_z28_2_w11:
**	mov	(w1[2-5]), w11
**	movaz	{z28\.s - z31\.s}, za2h\.s\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_f32_z28_2_w11, svfloat32x4_t,
		 z28 = svreadz_hor_za32_f32_vg4 (2, w11),
		 z28 = svreadz_hor_za32_f32_vg4 (2, w11))

/*
** readz_za32_s32_z0_3_w12:
**	movaz	{z0\.s - z3\.s}, za3h\.s\[w12, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_s32_z0_3_w12, svint32x4_t,
		 z0 = svreadz_hor_za32_s32_vg4 (3, w12),
		 z0 = svreadz_hor_za32_s32_vg4 (3, w12))

/*
** readz_za32_u32_z18_0_w15:
**	movaz	{[^\n]+}, za0h\.s\[w15, 0:3\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z18_0_w15, svuint32x4_t,
		 z18 = svreadz_hor_za32_u32_vg4 (0, w15),
		 z18 = svreadz_hor_za32_u32_vg4 (0, w15))

/*
** readz_za32_f32_z23_1_w12p4:
**	add	(w[0-9]+), w12, #?4
**	movaz	{[^\n]+}, za1h\.s\[\1, 0:3\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (readz_za32_f32_z23_1_w12p4, svfloat32x4_t,
		 z23 = svreadz_hor_za32_f32_vg4 (1, w12 + 4),
		 z23 = svreadz_hor_za32_f32_vg4 (1, w12 + 4))

/*
** readz_za32_u32_z4_2_w12p1:
**	add	(w[0-9]+), w12, #?1
**	movaz	{z4\.s - z7\.s}, za2h\.s\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z4_2_w12p1, svuint32x4_t,
		 z4 = svreadz_hor_za32_u32_vg4 (2, w12 + 1),
		 z4 = svreadz_hor_za32_u32_vg4 (2, w12 + 1))

/*
** readz_za32_s32_z28_3_w12p2:
**	add	(w[0-9]+), w12, #?2
**	movaz	{z28\.s - z31\.s}, za3h\.s\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_s32_z28_3_w12p2, svint32x4_t,
		 z28 = svreadz_hor_za32_s32_vg4 (3, w12 + 2),
		 z28 = svreadz_hor_za32_s32_vg4 (3, w12 + 2))

/*
** readz_za32_f32_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	movaz	{z0\.s - z3\.s}, za0h\.s\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_f32_z0_0_w15p3, svfloat32x4_t,
		 z0 = svreadz_hor_za32_f32_vg4 (0, w15 + 3),
		 z0 = svreadz_hor_za32_f32_vg4 (0, w15 + 3))

/*
** readz_za32_u32_z28_1_w12p4:
**	add	(w[0-9]+), w12, #?4
**	movaz	{z28\.s - z31\.s}, za1h\.s\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z28_1_w12p4, svuint32x4_t,
		 z28 = svreadz_hor_za32_u32_vg4 (1, w12 + 4),
		 z28 = svreadz_hor_za32_u32_vg4 (1, w12 + 4))

/*
** readz_za32_f32_z4_2_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	movaz	{z4\.s - z7\.s}, za2h\.s\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_f32_z4_2_w12m1, svfloat32x4_t,
		 z4 = svreadz_hor_za32_f32_vg4 (2, w12 - 1),
		 z4 = svreadz_hor_za32_f32_vg4 (2, w12 - 1))

/*
** readz_za32_u32_z28_3_w16:
**	mov	(w1[2-5]), w16
**	movaz	{z28\.s - z31\.s}, za3h\.s\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z28_3_w16, svuint32x4_t,
		 z28 = svreadz_hor_za32_u32_vg4 (3, w16),
		 z28 = svreadz_hor_za32_u32_vg4 (3, w16))
