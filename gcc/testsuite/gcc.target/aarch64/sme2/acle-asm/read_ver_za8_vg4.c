/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** read_za8_s8_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	{z0\.b - z3\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z0_0_0, svint8x4_t,
		 z0 = svread_ver_za8_s8_vg4 (0, 0),
		 z0 = svread_ver_za8_s8_vg4 (0, 0))

/*
** read_za8_u8_z4_0_1:
**	mov	(w1[2-5]), #?1
**	mova	{z4\.b - z7\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z4_0_1, svuint8x4_t,
		 z4 = svread_ver_za8_u8_vg4 (0, 1),
		 z4 = svread_ver_za8_u8_vg4 (0, 1))

/*
** read_za8_s8_z28_0_w11:
**	mov	(w1[2-5]), w11
**	mova	{z28\.b - z31\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z28_0_w11, svint8x4_t,
		 z28 = svread_ver_za8_s8_vg4 (0, w11),
		 z28 = svread_ver_za8_s8_vg4 (0, w11))

/*
** read_za8_s8_z0_0_w12:
**	mova	{z0\.b - z3\.b}, za0v\.b\[w12, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z0_0_w12, svint8x4_t,
		 z0 = svread_ver_za8_s8_vg4 (0, w12),
		 z0 = svread_ver_za8_s8_vg4 (0, w12))

/*
** read_za8_u8_z18_0_w15:
**	mova	{[^\n]+}, za0v\.b\[w15, 0:3\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z18_0_w15, svuint8x4_t,
		 z18 = svread_ver_za8_u8_vg4 (0, w15),
		 z18 = svread_ver_za8_u8_vg4 (0, w15))

/*
** read_za8_s8_z23_0_w12p12:
**	mova	{[^\n]+}, za0v\.b\[w12, 12:15\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z23_0_w12p12, svint8x4_t,
		 z23 = svread_ver_za8_s8_vg4 (0, w12 + 12),
		 z23 = svread_ver_za8_s8_vg4 (0, w12 + 12))

/*
** read_za8_u8_z4_0_w12p1:
**	add	(w[0-9]+), w12, #?1
**	mova	{z4\.b - z7\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z4_0_w12p1, svuint8x4_t,
		 z4 = svread_ver_za8_u8_vg4 (0, w12 + 1),
		 z4 = svread_ver_za8_u8_vg4 (0, w12 + 1))

/*
** read_za8_s8_z28_0_w12p2:
**	add	(w[0-9]+), w12, #?2
**	mova	{z28\.b - z31\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z28_0_w12p2, svint8x4_t,
		 z28 = svread_ver_za8_s8_vg4 (0, w12 + 2),
		 z28 = svread_ver_za8_s8_vg4 (0, w12 + 2))

/*
** read_za8_u8_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	mova	{z0\.b - z3\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z0_0_w15p3, svuint8x4_t,
		 z0 = svread_ver_za8_u8_vg4 (0, w15 + 3),
		 z0 = svread_ver_za8_u8_vg4 (0, w15 + 3))

/*
** read_za8_u8_z0_0_w12p4:
**	mova	{z0\.b - z3\.b}, za0v\.b\[w12, 4:7\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z0_0_w12p4, svuint8x4_t,
		 z0 = svread_ver_za8_u8_vg4 (0, w12 + 4),
		 z0 = svread_ver_za8_u8_vg4 (0, w12 + 4))

/*
** read_za8_u8_z4_0_w15p12:
**	mova	{z4\.b - z7\.b}, za0v\.b\[w15, 12:15\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z4_0_w15p12, svuint8x4_t,
		 z4 = svread_ver_za8_u8_vg4 (0, w15 + 12),
		 z4 = svread_ver_za8_u8_vg4 (0, w15 + 12))

/*
** read_za8_u8_z28_0_w12p14:
**	add	(w[0-9]+), w12, #?14
**	mova	{z28\.b - z31\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z28_0_w12p14, svuint8x4_t,
		 z28 = svread_ver_za8_u8_vg4 (0, w12 + 14),
		 z28 = svread_ver_za8_u8_vg4 (0, w12 + 14))

/*
** read_za8_s8_z0_0_w15p16:
**	add	(w[0-9]+), w15, #?16
**	mova	{z0\.b - z3\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_s8_z0_0_w15p16, svint8x4_t,
		 z0 = svread_ver_za8_s8_vg4 (0, w15 + 16),
		 z0 = svread_ver_za8_s8_vg4 (0, w15 + 16))

/*
** read_za8_u8_z4_0_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	mova	{z4\.b - z7\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z4_0_w12m1, svuint8x4_t,
		 z4 = svread_ver_za8_u8_vg4 (0, w12 - 1),
		 z4 = svread_ver_za8_u8_vg4 (0, w12 - 1))

/*
** read_za8_u8_z28_0_w16:
**	mov	(w1[2-5]), w16
**	mova	{z28\.b - z31\.b}, za0v\.b\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za8_u8_z28_0_w16, svuint8x4_t,
		 z28 = svread_ver_za8_u8_vg4 (0, w16),
		 z28 = svread_ver_za8_u8_vg4 (0, w16))
