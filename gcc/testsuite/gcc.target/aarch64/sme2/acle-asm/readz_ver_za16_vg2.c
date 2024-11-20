/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_za16_s16_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	movaz	{z0\.h - z1\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_s16_z0_0_0, svint16x2_t,
		 z0 = svreadz_ver_za16_s16_vg2 (0, 0),
		 z0 = svreadz_ver_za16_s16_vg2 (0, 0))

/*
** readz_za16_u16_z4_1_1:
**	mov	(w1[2-5]), #?1
**	movaz	{z4\.h - z5\.h}, za1v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_u16_z4_1_1, svuint16x2_t,
		 z4 = svreadz_ver_za16_u16_vg2 (1, 1),
		 z4 = svreadz_ver_za16_u16_vg2 (1, 1))

/*
** readz_za16_f16_z28_0_w11:
**	mov	(w1[2-5]), w11
**	movaz	{z28\.h - z29\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_f16_z28_0_w11, svfloat16x2_t,
		 z28 = svreadz_ver_za16_f16_vg2 (0, w11),
		 z28 = svreadz_ver_za16_f16_vg2 (0, w11))

/*
** readz_za16_bf16_z0_1_w12:
**	movaz	{z0\.h - z1\.h}, za1v\.h\[w12, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_bf16_z0_1_w12, svbfloat16x2_t,
		 z0 = svreadz_ver_za16_bf16_vg2 (1, w12),
		 z0 = svreadz_ver_za16_bf16_vg2 (1, w12))

/*
** readz_za16_u16_z18_0_w15:
**	movaz	{z18\.h - z19\.h}, za0v\.h\[w15, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_u16_z18_0_w15, svuint16x2_t,
		 z18 = svreadz_ver_za16_u16_vg2 (0, w15),
		 z18 = svreadz_ver_za16_u16_vg2 (0, w15))

/*
** readz_za16_s16_z23_1_w12p6:
**	movaz	{[^\n]+}, za1v\.h\[w12, 6:7\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (readz_za16_s16_z23_1_w12p6, svint16x2_t,
		 z23 = svreadz_ver_za16_s16_vg2 (1, w12 + 6),
		 z23 = svreadz_ver_za16_s16_vg2 (1, w12 + 6))

/*
** readz_za16_f16_z4_0_w12p1:
**	add	(w[0-9]+), w12, #?1
**	movaz	{z4\.h - z5\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_f16_z4_0_w12p1, svfloat16x2_t,
		 z4 = svreadz_ver_za16_f16_vg2 (0, w12 + 1),
		 z4 = svreadz_ver_za16_f16_vg2 (0, w12 + 1))

/*
** readz_za16_s16_z28_1_w12p2:
**	movaz	{z28\.h - z29\.h}, za1v\.h\[w12, 2:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_s16_z28_1_w12p2, svint16x2_t,
		 z28 = svreadz_ver_za16_s16_vg2 (1, w12 + 2),
		 z28 = svreadz_ver_za16_s16_vg2 (1, w12 + 2))

/*
** readz_za16_u16_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	movaz	{z0\.h - z1\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_u16_z0_0_w15p3, svuint16x2_t,
		 z0 = svreadz_ver_za16_u16_vg2 (0, w15 + 3),
		 z0 = svreadz_ver_za16_u16_vg2 (0, w15 + 3))

/*
** readz_za16_bf16_z4_1_w15p4:
**	movaz	{z4\.h - z5\.h}, za1v\.h\[w15, 4:5\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_bf16_z4_1_w15p4, svbfloat16x2_t,
		 z4 = svreadz_ver_za16_bf16_vg2 (1, w15 + 4),
		 z4 = svreadz_ver_za16_bf16_vg2 (1, w15 + 4))

/*
** readz_za16_u16_z28_0_w12p7:
**	add	(w[0-9]+), w12, #?7
**	movaz	{z28\.h - z29\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_u16_z28_0_w12p7, svuint16x2_t,
		 z28 = svreadz_ver_za16_u16_vg2 (0, w12 + 7),
		 z28 = svreadz_ver_za16_u16_vg2 (0, w12 + 7))

/*
** readz_za16_s16_z0_1_w15p8:
**	add	(w[0-9]+), w15, #?8
**	movaz	{z0\.h - z1\.h}, za1v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_s16_z0_1_w15p8, svint16x2_t,
		 z0 = svreadz_ver_za16_s16_vg2 (1, w15 + 8),
		 z0 = svreadz_ver_za16_s16_vg2 (1, w15 + 8))

/*
** readz_za16_u16_z4_0_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	movaz	{z4\.h - z5\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_u16_z4_0_w12m1, svuint16x2_t,
		 z4 = svreadz_ver_za16_u16_vg2 (0, w12 - 1),
		 z4 = svreadz_ver_za16_u16_vg2 (0, w12 - 1))

/*
** readz_za16_u16_z18_1_w16:
**	mov	(w1[2-5]), w16
**	movaz	{z18\.h - z19\.h}, za1v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za16_u16_z18_1_w16, svuint16x2_t,
		 z18 = svreadz_ver_za16_u16_vg2 (1, w16),
		 z18 = svreadz_ver_za16_u16_vg2 (1, w16))
