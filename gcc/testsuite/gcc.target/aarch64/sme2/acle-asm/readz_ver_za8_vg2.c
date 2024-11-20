/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_za8_s8_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	movaz	{z0\.b - z1\.b}, za0v\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_s8_z0_0_0, svint8x2_t,
		 z0 = svreadz_ver_za8_s8_vg2 (0, 0),
		 z0 = svreadz_ver_za8_s8_vg2 (0, 0))

/*
** readz_za8_u8_z4_0_1:
**	mov	(w1[2-5]), #?1
**	movaz	{z4\.b - z5\.b}, za0v\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_u8_z4_0_1, svuint8x2_t,
		 z4 = svreadz_ver_za8_u8_vg2 (0, 1),
		 z4 = svreadz_ver_za8_u8_vg2 (0, 1))

/*
** readz_za8_s8_z28_0_w11:
**	mov	(w1[2-5]), w11
**	movaz	{z28\.b - z29\.b}, za0v\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_s8_z28_0_w11, svint8x2_t,
		 z28 = svreadz_ver_za8_s8_vg2 (0, w11),
		 z28 = svreadz_ver_za8_s8_vg2 (0, w11))

/*
** readz_za8_s8_z0_0_w12:
**	movaz	{z0\.b - z1\.b}, za0v\.b\[w12, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_s8_z0_0_w12, svint8x2_t,
		 z0 = svreadz_ver_za8_s8_vg2 (0, w12),
		 z0 = svreadz_ver_za8_s8_vg2 (0, w12))

/*
** readz_za8_u8_z18_0_w15:
**	movaz	{z18\.b - z19\.b}, za0v\.b\[w15, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_u8_z18_0_w15, svuint8x2_t,
		 z18 = svreadz_ver_za8_u8_vg2 (0, w15),
		 z18 = svreadz_ver_za8_u8_vg2 (0, w15))

/*
** readz_za8_s8_z23_0_w12p14:
**	movaz	{[^\n]+}, za0v\.b\[w12, 14:15\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (readz_za8_s8_z23_0_w12p14, svint8x2_t,
		 z23 = svreadz_ver_za8_s8_vg2 (0, w12 + 14),
		 z23 = svreadz_ver_za8_s8_vg2 (0, w12 + 14))

/*
** readz_za8_u8_z4_0_w12p1:
**	add	(w[0-9]+), w12, #?1
**	movaz	{z4\.b - z5\.b}, za0v\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_u8_z4_0_w12p1, svuint8x2_t,
		 z4 = svreadz_ver_za8_u8_vg2 (0, w12 + 1),
		 z4 = svreadz_ver_za8_u8_vg2 (0, w12 + 1))

/*
** readz_za8_s8_z28_0_w12p2:
**	movaz	{z28\.b - z29\.b}, za0v\.b\[w12, 2:3\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_s8_z28_0_w12p2, svint8x2_t,
		 z28 = svreadz_ver_za8_s8_vg2 (0, w12 + 2),
		 z28 = svreadz_ver_za8_s8_vg2 (0, w12 + 2))

/*
** readz_za8_u8_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	movaz	{z0\.b - z1\.b}, za0v\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_u8_z0_0_w15p3, svuint8x2_t,
		 z0 = svreadz_ver_za8_u8_vg2 (0, w15 + 3),
		 z0 = svreadz_ver_za8_u8_vg2 (0, w15 + 3))

/*
** readz_za8_u8_z4_0_w15p12:
**	movaz	{z4\.b - z5\.b}, za0v\.b\[w15, 12:13\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_u8_z4_0_w15p12, svuint8x2_t,
		 z4 = svreadz_ver_za8_u8_vg2 (0, w15 + 12),
		 z4 = svreadz_ver_za8_u8_vg2 (0, w15 + 12))

/*
** readz_za8_u8_z28_0_w12p15:
**	add	(w[0-9]+), w12, #?15
**	movaz	{z28\.b - z29\.b}, za0v\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_u8_z28_0_w12p15, svuint8x2_t,
		 z28 = svreadz_ver_za8_u8_vg2 (0, w12 + 15),
		 z28 = svreadz_ver_za8_u8_vg2 (0, w12 + 15))

/*
** readz_za8_s8_z0_0_w15p16:
**	add	(w[0-9]+), w15, #?16
**	movaz	{z0\.b - z1\.b}, za0v\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_s8_z0_0_w15p16, svint8x2_t,
		 z0 = svreadz_ver_za8_s8_vg2 (0, w15 + 16),
		 z0 = svreadz_ver_za8_s8_vg2 (0, w15 + 16))

/*
** readz_za8_u8_z4_0_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	movaz	{z4\.b - z5\.b}, za0v\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_u8_z4_0_w12m1, svuint8x2_t,
		 z4 = svreadz_ver_za8_u8_vg2 (0, w12 - 1),
		 z4 = svreadz_ver_za8_u8_vg2 (0, w12 - 1))

/*
** readz_za8_u8_z18_0_w16:
**	mov	(w1[2-5]), w16
**	movaz	{z18\.b - z19\.b}, za0v\.b\[\1, 0:1\]
**	ret
*/
TEST_READ_ZA_XN (readz_za8_u8_z18_0_w16, svuint8x2_t,
		 z18 = svreadz_ver_za8_u8_vg2 (0, w16),
		 z18 = svreadz_ver_za8_u8_vg2 (0, w16))
