/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vtbx1_s8:
**	fmov	d[0-9]+, d[0-9]+
**	movi	v[0-9]+\.8b, 0x7
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	cmhs	v[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b
**	bit	v[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx1_s8, int8x8_t, int8x8_t, int8x8_t, int8x8_t)

/*
** test_vtbx1_u8:
**	fmov	d[0-9]+, d[0-9]+
**	movi	v[0-9]+\.8b, 0x7
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	cmhs	v[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b
**	bit	v[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx1_u8, uint8x8_t, uint8x8_t, uint8x8_t, uint8x8_t)

/*
** test_vtbx1_p8:
**	fmov	d[0-9]+, d[0-9]+
**	movi	v[0-9]+\.8b, 0x7
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	cmhs	v[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b
**	bit	v[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx1_p8, poly8x8_t, poly8x8_t, poly8x8_t, uint8x8_t)

/*
** test_vtbx1_mf8:
**	fmov	d[0-9]+, d[0-9]+
**	movi	v[0-9]+\.8b, 0x7
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	cmhs	v[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b
**	bit	v[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx1_mf8, mfloat8x8_t, mfloat8x8_t, mfloat8x8_t, uint8x8_t)


/*
** test_vtbx2_s8:
**	sub	sp, sp, #16
**	stp	d[0-9]+, d[0-9]+, \[sp\]
**	ldr	q[0-9]+, \[sp\]
**	add	sp, sp, 16
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx2_s8, int8x8_t, int8x8_t, int8x8x2_t, int8x8_t)

/*
** test_vtbx2_u8:
**	sub	sp, sp, #16
**	stp	d[0-9]+, d[0-9]+, \[sp\]
**	ldr	q[0-9]+, \[sp\]
**	add	sp, sp, 16
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx2_u8, uint8x8_t, uint8x8_t, uint8x8x2_t, uint8x8_t)

/*
** test_vtbx2_p8:
**	sub	sp, sp, #16
**	stp	d[0-9]+, d[0-9]+, \[sp\]
**	ldr	q[0-9]+, \[sp\]
**	add	sp, sp, 16
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx2_p8, poly8x8_t, poly8x8_t, poly8x8x2_t, uint8x8_t)

/*
** test_vtbx2_mf8:
**	sub	sp, sp, #16
**	stp	d[0-9]+, d[0-9]+, \[sp\]
**	ldr	q[0-9]+, \[sp\]
**	add	sp, sp, 16
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx2_mf8, mfloat8x8_t, mfloat8x8_t, mfloat8x8x2_t, uint8x8_t)

/*
** test_vtbx3_s8:
**	fmov	d([0-9]+), d[0-9]+
**	zip1	v([0-9]+)\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbl	v\2\.8b, {v\2\.16b - v\1\.16b}, v[0-9]+\.8b
**	movi	v\1\.8b, 0x17
**	cmhs	v\1\.8b, v\1\.8b, v[0-9]+\.8b
**	bit	v[0-9]+\.8b, v\2\.8b, v\1\.8b
**	ret
*/
TEST_TERNARY (vtbx3_s8, int8x8_t, int8x8_t, int8x8x3_t, int8x8_t)

/*
** test_vtbx3_u8:
**	fmov	d([0-9]+), d[0-9]+
**	zip1	v([0-9]+)\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbl	v\2\.8b, {v\2\.16b - v\1\.16b}, v[0-9]+\.8b
**	movi	v\1\.8b, 0x17
**	cmhs	v\1\.8b, v\1\.8b, v[0-9]+\.8b
**	bit	v[0-9]+\.8b, v\2\.8b, v\1\.8b
**	ret
*/
TEST_TERNARY (vtbx3_u8, uint8x8_t, uint8x8_t, uint8x8x3_t, uint8x8_t)

/*
** test_vtbx3_p8:
**	fmov	d([0-9]+), d[0-9]+
**	zip1	v([0-9]+)\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbl	v\2\.8b, {v\2\.16b - v\1\.16b}, v[0-9]+\.8b
**	movi	v\1\.8b, 0x17
**	cmhs	v\1\.8b, v\1\.8b, v[0-9]+\.8b
**	bit	v[0-9]+\.8b, v\2\.8b, v\1\.8b
**	ret
*/
TEST_TERNARY (vtbx3_p8, poly8x8_t, poly8x8_t, poly8x8x3_t, uint8x8_t)

/*
** test_vtbx3_mf8:
**	fmov	d([0-9]+), d[0-9]+
**	zip1	v([0-9]+)\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbl	v\2\.8b, {v\2\.16b - v\1\.16b}, v[0-9]+\.8b
**	movi	v\1\.8b, 0x17
**	cmhs	v\1\.8b, v\1\.8b, v[0-9]+\.8b
**	bit	v[0-9]+\.8b, v\2\.8b, v\1\.8b
**	ret
*/
TEST_TERNARY (vtbx3_mf8, mfloat8x8_t, mfloat8x8_t, mfloat8x8x3_t, uint8x8_t)

/*
** test_vtbx4_s8:
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx4_s8, int8x8_t, int8x8_t, int8x8x4_t, int8x8_t)

/*
** test_vtbx4_u8:
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx4_u8, uint8x8_t, uint8x8_t, uint8x8x4_t, uint8x8_t)

/*
** test_vtbx4_p8:
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx4_p8, poly8x8_t, poly8x8_t, poly8x8x4_t, uint8x8_t)

/*
** test_vtbx4_mf8:
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vtbx4_mf8, mfloat8x8_t, mfloat8x8_t, mfloat8x8x4_t, uint8x8_t)

/*
** test_vqtbx1_s8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx1_s8, int8x8_t, int8x8_t, int8x16_t, uint8x8_t)

/*
** test_vqtbx1_u8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx1_u8, uint8x8_t, uint8x8_t, uint8x16_t, uint8x8_t)

/*
** test_vqtbx1_p8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx1_p8, poly8x8_t, poly8x8_t, poly8x16_t, uint8x8_t)

/*
** test_vqtbx1_mf8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx1_mf8, mfloat8x8_t, mfloat8x8_t, mfloat8x16_t,
	      uint8x8_t)

/*
** test_vqtbx2_s8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx2_s8, int8x8_t, int8x8_t, int8x16x2_t, uint8x8_t)

/*
** test_vqtbx2_u8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx2_u8, uint8x8_t, uint8x8_t, uint8x16x2_t, uint8x8_t)

/*
** test_vqtbx2_p8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx2_p8, poly8x8_t, poly8x8_t, poly8x16x2_t, uint8x8_t)

/*
** test_vqtbx2_mf8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx2_mf8, mfloat8x8_t, mfloat8x8_t, mfloat8x16x2_t,
	      uint8x8_t)

/*
** test_vqtbx3_s8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx3_s8, int8x8_t, int8x8_t, int8x16x3_t, uint8x8_t)

/*
** test_vqtbx3_u8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx3_u8, uint8x8_t, uint8x8_t, uint8x16x3_t, uint8x8_t)

/*
** test_vqtbx3_p8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx3_p8, poly8x8_t, poly8x8_t, poly8x16x3_t, uint8x8_t)

/*
** test_vqtbx3_mf8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx3_mf8, mfloat8x8_t, mfloat8x8_t, mfloat8x16x3_t,
	      uint8x8_t)

/*
** test_vqtbx4_s8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx4_s8, int8x8_t, int8x8_t, int8x16x4_t, uint8x8_t)

/*
** test_vqtbx4_u8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx4_u8, uint8x8_t, uint8x8_t, uint8x16x4_t, uint8x8_t)

/*
** test_vqtbx4_p8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx4_p8, poly8x8_t, poly8x8_t, poly8x16x4_t, uint8x8_t)

/*
** test_vqtbx4_mf8:
**	tbx	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_TERNARY (vqtbx4_mf8, mfloat8x8_t, mfloat8x8_t, mfloat8x16x4_t,
	      uint8x8_t)

/*
** test_vqtbx1q_s8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx1q_s8, int8x16_t, int8x16_t, int8x16_t, uint8x16_t)

/*
** test_vqtbx1q_u8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx1q_u8, uint8x16_t, uint8x16_t, uint8x16_t, uint8x16_t)

/*
** test_vqtbx1q_p8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx1q_p8, poly8x16_t, poly8x16_t, poly8x16_t, uint8x16_t)

/*
** test_vqtbx1q_mf8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx1q_mf8, mfloat8x16_t, mfloat8x16_t, mfloat8x16_t, uint8x16_t)

/*
** test_vqtbx2q_s8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx2q_s8, int8x16_t, int8x16_t, int8x16x2_t, uint8x16_t)

/*
** test_vqtbx2q_u8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx2q_u8, uint8x16_t, uint8x16_t, uint8x16x2_t, uint8x16_t)

/*
** test_vqtbx2q_p8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx2q_p8, poly8x16_t, poly8x16_t, poly8x16x2_t, uint8x16_t)

/*
** test_vqtbx2q_mf8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx2q_mf8, mfloat8x16_t, mfloat8x16_t, mfloat8x16x2_t, uint8x16_t)

/*
** test_vqtbx3q_s8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx3q_s8, int8x16_t, int8x16_t, int8x16x3_t, uint8x16_t)

/*
** test_vqtbx3q_u8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx3q_u8, uint8x16_t, uint8x16_t, uint8x16x3_t, uint8x16_t)

/*
** test_vqtbx3q_p8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx3q_p8, poly8x16_t, poly8x16_t, poly8x16x3_t, uint8x16_t)

/*
** test_vqtbx3q_mf8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx3q_mf8, mfloat8x16_t, mfloat8x16_t, mfloat8x16x3_t, uint8x16_t)

/*
** test_vqtbx4q_s8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx4q_s8, int8x16_t, int8x16_t, int8x16x4_t, uint8x16_t)

/*
** test_vqtbx4q_u8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx4q_u8, uint8x16_t, uint8x16_t, uint8x16x4_t, uint8x16_t)

/*
** test_vqtbx4q_p8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx4q_p8, poly8x16_t, poly8x16_t, poly8x16x4_t, uint8x16_t)

/*
** test_vqtbx4q_mf8:
**	tbx	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_TERNARY (vqtbx4q_mf8, mfloat8x16_t, mfloat8x16_t, mfloat8x16x4_t, uint8x16_t)
