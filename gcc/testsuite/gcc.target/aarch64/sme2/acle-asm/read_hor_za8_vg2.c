/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** read_za8_s8_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	{z0\.b - z1\.b}, za0h\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z0_0_0, svint8x2_t,
		 z0 = svread_hor_za8_s8_vg2 (0, 0),
		 z0 = svread_hor_za8_s8_vg2 (0, 0))

/*
** read_za8_u8_z4_0_1:
**	mov	(w1[2-5]), #?1
**	mova	{z4\.b - z5\.b}, za0h\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z4_0_1, svuint8x2_t,
		 z4 = svread_hor_za8_u8_vg2 (0, 1),
		 z4 = svread_hor_za8_u8_vg2 (0, 1))

/*
** read_za8_s8_z28_0_w11:
**	mov	(w1[2-5]), w11
**	mova	{z28\.b - z29\.b}, za0h\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z28_0_w11, svint8x2_t,
		 z28 = svread_hor_za8_s8_vg2 (0, w11),
		 z28 = svread_hor_za8_s8_vg2 (0, w11))

/*
** read_za8_s8_z0_0_w12:
**	mova	{z0\.b - z1\.b}, za0h\.b\[w12, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z0_0_w12, svint8x2_t,
		 z0 = svread_hor_za8_s8_vg2 (0, w12),
		 z0 = svread_hor_za8_s8_vg2 (0, w12))

/*
** read_za8_u8_z18_0_w15:
**	mova	{z18\.b - z19\.b}, za0h\.b\[w15, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z18_0_w15, svuint8x2_t,
		 z18 = svread_hor_za8_u8_vg2 (0, w15),
		 z18 = svread_hor_za8_u8_vg2 (0, w15))

/*
** read_za8_s8_z23_0_w12p14:
**	mova	{[^\n]+}, za0h\.b\[w12, 14:15\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z23_0_w12p14, svint8x2_t,
		 z23 = svread_hor_za8_s8_vg2 (0, w12 + 14),
		 z23 = svread_hor_za8_s8_vg2 (0, w12 + 14))

/*
** read_za8_u8_z4_0_w12p1:
**	add	(w[0-9]+), w12, #?1
**	mova	{z4\.b - z5\.b}, za0h\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z4_0_w12p1, svuint8x2_t,
		 z4 = svread_hor_za8_u8_vg2 (0, w12 + 1),
		 z4 = svread_hor_za8_u8_vg2 (0, w12 + 1))

/*
** read_za8_s8_z28_0_w12p2:
**	mova	{z28\.b - z29\.b}, za0h\.b\[w12, 2:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z28_0_w12p2, svint8x2_t,
		 z28 = svread_hor_za8_s8_vg2 (0, w12 + 2),
		 z28 = svread_hor_za8_s8_vg2 (0, w12 + 2))

/*
** read_za8_u8_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	mova	{z0\.b - z1\.b}, za0h\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z0_0_w15p3, svuint8x2_t,
		 z0 = svread_hor_za8_u8_vg2 (0, w15 + 3),
		 z0 = svread_hor_za8_u8_vg2 (0, w15 + 3))

/*
** read_za8_u8_z4_0_w15p12:
**	mova	{z4\.b - z5\.b}, za0h\.b\[w15, 12:13\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z4_0_w15p12, svuint8x2_t,
		 z4 = svread_hor_za8_u8_vg2 (0, w15 + 12),
		 z4 = svread_hor_za8_u8_vg2 (0, w15 + 12))

/*
** read_za8_u8_z28_0_w12p15:
**	add	(w[0-9]+), w12, #?15
**	mova	{z28\.b - z29\.b}, za0h\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z28_0_w12p15, svuint8x2_t,
		 z28 = svread_hor_za8_u8_vg2 (0, w12 + 15),
		 z28 = svread_hor_za8_u8_vg2 (0, w12 + 15))

/*
** read_za8_s8_z0_0_w15p16:
**	add	(w[0-9]+), w15, #?16
**	mova	{z0\.b - z1\.b}, za0h\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z0_0_w15p16, svint8x2_t,
		 z0 = svread_hor_za8_s8_vg2 (0, w15 + 16),
		 z0 = svread_hor_za8_s8_vg2 (0, w15 + 16))

/*
** read_za8_u8_z4_0_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	mova	{z4\.b - z5\.b}, za0h\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z4_0_w12m1, svuint8x2_t,
		 z4 = svread_hor_za8_u8_vg2 (0, w12 - 1),
		 z4 = svread_hor_za8_u8_vg2 (0, w12 - 1))

/*
** read_za8_u8_z18_0_w16:
**	mov	(w1[2-5]), w16
**	mova	{z18\.b - z19\.b}, za0h\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z18_0_w16, svuint8x2_t,
		 z18 = svread_hor_za8_u8_vg2 (0, w16),
		 z18 = svread_hor_za8_u8_vg2 (0, w16))
