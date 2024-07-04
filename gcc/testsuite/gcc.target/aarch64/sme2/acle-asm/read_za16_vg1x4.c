/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** read_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	mova	{z0\.d - z3\.d}, za\.d\[\1, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_0_z0, svint16x4_t,
		 z0 = svread_za16_s16_vg1x4 (0),
		 z0 = svread_za16_s16_vg1x4 (0))

/*
** read_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	mova	{z0\.d - z3\.d}, za\.d\[\1, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w0_z0, svuint16x4_t,
		 z0 = svread_za16_u16_vg1x4 (w0),
		 z0 = svread_za16_u16_vg1x4 (w0))

/*
** read_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	mova	{z0\.d - z3\.d}, za\.d\[\1, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w7_z0, svfloat16x4_t,
		 z0 = svread_za16_f16_vg1x4 (w7),
		 z0 = svread_za16_f16_vg1x4 (w7))

/*
** read_w8_z0:
**	mova	{z0\.d - z3\.d}, za\.d\[w8, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w8_z0, svint16x4_t,
		 z0 = svread_za16_s16_vg1x4 (w8),
		 z0 = svread_za16_s16_vg1x4 (w8))

/*
** read_w11_z0:
**	mova	{z0\.d - z3\.d}, za\.d\[w11, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w11_z0, svuint16x4_t,
		 z0 = svread_za16_u16_vg1x4 (w11),
		 z0 = svread_za16_u16_vg1x4 (w11))


/*
** read_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	mova	{z0\.d - z3\.d}, za\.d\[\1, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w12_z0, svbfloat16x4_t,
		 z0 = svread_za16_bf16_vg1x4 (w12),
		 z0 = svread_za16_bf16_vg1x4 (w12))

/*
** read_w8p7_z0:
**	mova	{z0\.d - z3\.d}, za\.d\[w8, 7, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w8p7_z0, svint16x4_t,
		 z0 = svread_za16_s16_vg1x4 (w8 + 7),
		 z0 = svread_za16_s16_vg1x4 (w8 + 7))

/*
** read_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	mova	{z0\.d - z3\.d}, za\.d\[\1, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w8p8_z0, svuint16x4_t,
		 z0 = svread_za16_u16_vg1x4 (w8 + 8),
		 z0 = svread_za16_u16_vg1x4 (w8 + 8))

/*
** read_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	mova	{z0\.d - z3\.d}, za\.d\[\1, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w8m1_z0, svfloat16x4_t,
		 z0 = svread_za16_f16_vg1x4 (w8 - 1),
		 z0 = svread_za16_f16_vg1x4 (w8 - 1))

/*
** read_w8_z4:
**	mova	{z4\.d - z7\.d}, za\.d\[w8, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w8_z4, svint16x4_t,
		 z4 = svread_za16_s16_vg1x4 (w8),
		 z4 = svread_za16_s16_vg1x4 (w8))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** read_w8_z18:
**	mova	[^\n]+, za\.d\[w8, 0, vgx4\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_w8_z18, svuint16x4_t,
		 z18 = svread_za16_u16_vg1x4 (w8),
		 z18 = svread_za16_u16_vg1x4 (w8))

/*
** read_w8_z23:
**	mova	[^\n]+, za\.d\[w8, 0, vgx4\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_w8_z23, svbfloat16x4_t,
		 z23 = svread_za16_bf16_vg1x4 (w8),
		 z23 = svread_za16_bf16_vg1x4 (w8))

/*
** read_w8_z28:
**	mova	{z28\.d - z31\.d}, za\.d\[w8, 0, vgx4\]
**	ret
*/
TEST_READ_ZA_XN (read_w8_z28, svint16x4_t,
		 z28 = svread_za16_s16_vg1x4 (w8),
		 z28 = svread_za16_s16_vg1x4 (w8))
