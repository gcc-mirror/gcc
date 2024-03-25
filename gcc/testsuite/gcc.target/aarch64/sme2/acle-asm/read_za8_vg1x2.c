/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** read_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	mova	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_0_z0, svint8x2_t,
		 z0 = svread_za8_s8_vg1x2 (0),
		 z0 = svread_za8_s8_vg1x2 (0))

/*
** read_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	mova	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w0_z0, svint8x2_t,
		 z0 = svread_za8_s8_vg1x2 (w0),
		 z0 = svread_za8_s8_vg1x2 (w0))

/*
** read_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	mova	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w7_z0, svuint8x2_t,
		 z0 = svread_za8_u8_vg1x2 (w7),
		 z0 = svread_za8_u8_vg1x2 (w7))

/*
** read_w8_z0:
**	mova	{z0\.d - z1\.d}, za\.d\[w8, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w8_z0, svint8x2_t,
		 z0 = svread_za8_s8_vg1x2 (w8),
		 z0 = svread_za8_s8_vg1x2 (w8))

/*
** read_w11_z0:
**	mova	{z0\.d - z1\.d}, za\.d\[w11, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w11_z0, svint8x2_t,
		 z0 = svread_za8_s8_vg1x2 (w11),
		 z0 = svread_za8_s8_vg1x2 (w11))


/*
** read_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	mova	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w12_z0, svuint8x2_t,
		 z0 = svread_za8_u8_vg1x2 (w12),
		 z0 = svread_za8_u8_vg1x2 (w12))

/*
** read_w8p7_z0:
**	mova	{z0\.d - z1\.d}, za\.d\[w8, 7, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w8p7_z0, svint8x2_t,
		 z0 = svread_za8_s8_vg1x2 (w8 + 7),
		 z0 = svread_za8_s8_vg1x2 (w8 + 7))

/*
** read_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	mova	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w8p8_z0, svint8x2_t,
		 z0 = svread_za8_s8_vg1x2 (w8 + 8),
		 z0 = svread_za8_s8_vg1x2 (w8 + 8))

/*
** read_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	mova	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w8m1_z0, svuint8x2_t,
		 z0 = svread_za8_u8_vg1x2 (w8 - 1),
		 z0 = svread_za8_u8_vg1x2 (w8 - 1))

/*
** read_w8_z18:
**	mova	{z18\.d - z19\.d}, za\.d\[w8, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w8_z18, svuint8x2_t,
		 z18 = svread_za8_u8_vg1x2 (w8),
		 z18 = svread_za8_u8_vg1x2 (w8))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** read_w8_z23:
**	mova	[^\n]+, za\.d\[w8, 0, vgx2\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_w8_z23, svint8x2_t,
		 z23 = svread_za8_s8_vg1x2 (w8),
		 z23 = svread_za8_s8_vg1x2 (w8))

/*
** read_w8_z28:
**	mova	{z28\.d - z29\.d}, za\.d\[w8, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (read_w8_z28, svuint8x2_t,
		 z28 = svread_za8_u8_vg1x2 (w8),
		 z28 = svread_za8_u8_vg1x2 (w8))
