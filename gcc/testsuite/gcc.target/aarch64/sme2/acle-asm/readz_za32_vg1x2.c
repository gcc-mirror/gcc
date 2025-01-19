/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	movaz	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_0_z0, svfloat32x2_t,
		 z0 = svreadz_za32_f32_vg1x2 (0),
		 z0 = svreadz_za32_f32_vg1x2 (0))

/*
** readz_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	movaz	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w0_z0, svint32x2_t,
		 z0 = svreadz_za32_s32_vg1x2 (w0),
		 z0 = svreadz_za32_s32_vg1x2 (w0))

/*
** readz_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	movaz	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w7_z0, svuint32x2_t,
		 z0 = svreadz_za32_u32_vg1x2 (w7),
		 z0 = svreadz_za32_u32_vg1x2 (w7))

/*
** readz_w8_z0:
**	movaz	{z0\.d - z1\.d}, za\.d\[w8, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w8_z0, svfloat32x2_t,
		 z0 = svreadz_za32_f32_vg1x2 (w8),
		 z0 = svreadz_za32_f32_vg1x2 (w8))

/*
** readz_w11_z0:
**	movaz	{z0\.d - z1\.d}, za\.d\[w11, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w11_z0, svint32x2_t,
		 z0 = svreadz_za32_s32_vg1x2 (w11),
		 z0 = svreadz_za32_s32_vg1x2 (w11))


/*
** readz_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	movaz	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w12_z0, svuint32x2_t,
		 z0 = svreadz_za32_u32_vg1x2 (w12),
		 z0 = svreadz_za32_u32_vg1x2 (w12))

/*
** readz_w8p7_z0:
**	movaz	{z0\.d - z1\.d}, za\.d\[w8, 7, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w8p7_z0, svfloat32x2_t,
		 z0 = svreadz_za32_f32_vg1x2 (w8 + 7),
		 z0 = svreadz_za32_f32_vg1x2 (w8 + 7))

/*
** readz_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	movaz	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w8p8_z0, svint32x2_t,
		 z0 = svreadz_za32_s32_vg1x2 (w8 + 8),
		 z0 = svreadz_za32_s32_vg1x2 (w8 + 8))

/*
** readz_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	movaz	{z0\.d - z1\.d}, za\.d\[\1, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w8m1_z0, svuint32x2_t,
		 z0 = svreadz_za32_u32_vg1x2 (w8 - 1),
		 z0 = svreadz_za32_u32_vg1x2 (w8 - 1))

/*
** readz_w8_z18:
**	movaz	{z18\.d - z19\.d}, za\.d\[w8, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w8_z18, svfloat32x2_t,
		 z18 = svreadz_za32_f32_vg1x2 (w8),
		 z18 = svreadz_za32_f32_vg1x2 (w8))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** readz_w8_z23:
**	movaz	[^\n]+, za\.d\[w8, 0, vgx2\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (readz_w8_z23, svint32x2_t,
		 z23 = svreadz_za32_s32_vg1x2 (w8),
		 z23 = svreadz_za32_s32_vg1x2 (w8))

/*
** readz_w8_z28:
**	movaz	{z28\.d - z29\.d}, za\.d\[w8, 0, vgx2\]
**	ret
*/
TEST_READ_ZA_XN (readz_w8_z28, svuint32x2_t,
		 z28 = svreadz_za32_u32_vg1x2 (w8),
		 z28 = svreadz_za32_u32_vg1x2 (w8))
