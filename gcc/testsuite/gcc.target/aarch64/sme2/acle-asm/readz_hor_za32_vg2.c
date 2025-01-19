/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_za32_s32_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	movaz	{z0\.s - z1\.s}, za0h\.s\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_s32_z0_0_0, svint32x2_t,
		 z0 = svreadz_hor_za32_s32_vg2 (0, 0),
		 z0 = svreadz_hor_za32_s32_vg2 (0, 0))

/*
** readz_za32_u32_z4_1_1:
**	mov	(w1[2-5]), #?1
**	movaz	{z4\.s - z5\.s}, za1h\.s\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z4_1_1, svuint32x2_t,
		 z4 = svreadz_hor_za32_u32_vg2 (1, 1),
		 z4 = svreadz_hor_za32_u32_vg2 (1, 1))

/*
** readz_za32_f32_z28_2_w11:
**	mov	(w1[2-5]), w11
**	movaz	{z28\.s - z29\.s}, za2h\.s\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_f32_z28_2_w11, svfloat32x2_t,
		 z28 = svreadz_hor_za32_f32_vg2 (2, w11),
		 z28 = svreadz_hor_za32_f32_vg2 (2, w11))

/*
** readz_za32_f32_z0_3_w12:
**	movaz	{z0\.s - z1\.s}, za3h\.s\[w12, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_f32_z0_3_w12, svfloat32x2_t,
		 z0 = svreadz_hor_za32_f32_vg2 (3, w12),
		 z0 = svreadz_hor_za32_f32_vg2 (3, w12))

/*
** readz_za32_u32_z18_0_w15:
**	movaz	{z18\.s - z19\.s}, za0h\.s\[w15, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z18_0_w15, svuint32x2_t,
		 z18 = svreadz_hor_za32_u32_vg2 (0, w15),
		 z18 = svreadz_hor_za32_u32_vg2 (0, w15))

/*
** readz_za32_s32_z23_1_w12p2:
**	movaz	{[^\n]+}, za1h\.s\[w12, 2:3\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (readz_za32_s32_z23_1_w12p2, svint32x2_t,
		 z23 = svreadz_hor_za32_s32_vg2 (1, w12 + 2),
		 z23 = svreadz_hor_za32_s32_vg2 (1, w12 + 2))

/*
** readz_za32_f32_z4_2_w12p1:
**	add	(w[0-9]+), w12, #?1
**	movaz	{z4\.s - z5\.s}, za2h\.s\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_f32_z4_2_w12p1, svfloat32x2_t,
		 z4 = svreadz_hor_za32_f32_vg2 (2, w12 + 1),
		 z4 = svreadz_hor_za32_f32_vg2 (2, w12 + 1))

/*
** readz_za32_u32_z0_3_w15p3:
**	add	(w[0-9]+), w15, #?3
**	movaz	{z0\.s - z1\.s}, za3h\.s\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z0_3_w15p3, svuint32x2_t,
		 z0 = svreadz_hor_za32_u32_vg2 (3, w15 + 3),
		 z0 = svreadz_hor_za32_u32_vg2 (3, w15 + 3))

/*
** readz_za32_s32_z0_1_w15p4:
**	add	(w[0-9]+), w15, #?4
**	movaz	{z0\.s - z1\.s}, za1h\.s\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_s32_z0_1_w15p4, svint32x2_t,
		 z0 = svreadz_hor_za32_s32_vg2 (1, w15 + 4),
		 z0 = svreadz_hor_za32_s32_vg2 (1, w15 + 4))

/*
** readz_za32_u32_z4_3_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	movaz	{z4\.s - z5\.s}, za3h\.s\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z4_3_w12m1, svuint32x2_t,
		 z4 = svreadz_hor_za32_u32_vg2 (3, w12 - 1),
		 z4 = svreadz_hor_za32_u32_vg2 (3, w12 - 1))

/*
** readz_za32_u32_z18_1_w16:
**	mov	(w1[2-5]), w16
**	movaz	{z18\.s - z19\.s}, za1h\.s\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za32_u32_z18_1_w16, svuint32x2_t,
		 z18 = svreadz_hor_za32_u32_vg2 (1, w16),
		 z18 = svreadz_hor_za32_u32_vg2 (1, w16))
