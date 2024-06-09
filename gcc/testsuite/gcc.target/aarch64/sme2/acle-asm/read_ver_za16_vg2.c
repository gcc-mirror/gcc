/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** read_za16_s16_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	{z0\.h - z1\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_s16_z0_0_0, svint16x2_t,
		 z0 = svread_ver_za16_s16_vg2 (0, 0),
		 z0 = svread_ver_za16_s16_vg2 (0, 0))

/*
** read_za16_u16_z4_1_1:
**	mov	(w1[2-5]), #?1
**	mova	{z4\.h - z5\.h}, za1v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z4_1_1, svuint16x2_t,
		 z4 = svread_ver_za16_u16_vg2 (1, 1),
		 z4 = svread_ver_za16_u16_vg2 (1, 1))

/*
** read_za16_f16_z28_0_w11:
**	mov	(w1[2-5]), w11
**	mova	{z28\.h - z29\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_f16_z28_0_w11, svfloat16x2_t,
		 z28 = svread_ver_za16_f16_vg2 (0, w11),
		 z28 = svread_ver_za16_f16_vg2 (0, w11))

/*
** read_za16_bf16_z0_1_w12:
**	mova	{z0\.h - z1\.h}, za1v\.h\[w12, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_bf16_z0_1_w12, svbfloat16x2_t,
		 z0 = svread_ver_za16_bf16_vg2 (1, w12),
		 z0 = svread_ver_za16_bf16_vg2 (1, w12))

/*
** read_za16_u16_z18_0_w15:
**	mova	{z18\.h - z19\.h}, za0v\.h\[w15, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z18_0_w15, svuint16x2_t,
		 z18 = svread_ver_za16_u16_vg2 (0, w15),
		 z18 = svread_ver_za16_u16_vg2 (0, w15))

/*
** read_za16_s16_z23_1_w12p6:
**	mova	{[^\n]+}, za1v\.h\[w12, 6:7\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_za16_s16_z23_1_w12p6, svint16x2_t,
		 z23 = svread_ver_za16_s16_vg2 (1, w12 + 6),
		 z23 = svread_ver_za16_s16_vg2 (1, w12 + 6))

/*
** read_za16_f16_z4_0_w12p1:
**	add	(w[0-9]+), w12, #?1
**	mova	{z4\.h - z5\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_f16_z4_0_w12p1, svfloat16x2_t,
		 z4 = svread_ver_za16_f16_vg2 (0, w12 + 1),
		 z4 = svread_ver_za16_f16_vg2 (0, w12 + 1))

/*
** read_za16_s16_z28_1_w12p2:
**	mova	{z28\.h - z29\.h}, za1v\.h\[w12, 2:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_s16_z28_1_w12p2, svint16x2_t,
		 z28 = svread_ver_za16_s16_vg2 (1, w12 + 2),
		 z28 = svread_ver_za16_s16_vg2 (1, w12 + 2))

/*
** read_za16_u16_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	mova	{z0\.h - z1\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z0_0_w15p3, svuint16x2_t,
		 z0 = svread_ver_za16_u16_vg2 (0, w15 + 3),
		 z0 = svread_ver_za16_u16_vg2 (0, w15 + 3))

/*
** read_za16_bf16_z4_1_w15p4:
**	mova	{z4\.h - z5\.h}, za1v\.h\[w15, 4:5\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_bf16_z4_1_w15p4, svbfloat16x2_t,
		 z4 = svread_ver_za16_bf16_vg2 (1, w15 + 4),
		 z4 = svread_ver_za16_bf16_vg2 (1, w15 + 4))

/*
** read_za16_u16_z28_0_w12p7:
**	add	(w[0-9]+), w12, #?7
**	mova	{z28\.h - z29\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z28_0_w12p7, svuint16x2_t,
		 z28 = svread_ver_za16_u16_vg2 (0, w12 + 7),
		 z28 = svread_ver_za16_u16_vg2 (0, w12 + 7))

/*
** read_za16_s16_z0_1_w15p8:
**	add	(w[0-9]+), w15, #?8
**	mova	{z0\.h - z1\.h}, za1v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_s16_z0_1_w15p8, svint16x2_t,
		 z0 = svread_ver_za16_s16_vg2 (1, w15 + 8),
		 z0 = svread_ver_za16_s16_vg2 (1, w15 + 8))

/*
** read_za16_u16_z4_0_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	mova	{z4\.h - z5\.h}, za0v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z4_0_w12m1, svuint16x2_t,
		 z4 = svread_ver_za16_u16_vg2 (0, w12 - 1),
		 z4 = svread_ver_za16_u16_vg2 (0, w12 - 1))

/*
** read_za16_u16_z18_1_w16:
**	mov	(w1[2-5]), w16
**	mova	{z18\.h - z19\.h}, za1v\.h\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za16_u16_z18_1_w16, svuint16x2_t,
		 z18 = svread_ver_za16_u16_vg2 (1, w16),
		 z18 = svread_ver_za16_u16_vg2 (1, w16))
