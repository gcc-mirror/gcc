/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vtbl1_s8:
**	fmov	d[0-9]+, d[0-9]+
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl1_s8, int8x8_t, int8x8_t, int8x8_t)

/*
** test_vtbl1_u8:
**	fmov	d[0-9]+, d[0-9]+
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl1_u8, uint8x8_t, uint8x8_t, uint8x8_t)

/*
** test_vtbl1_p8:
**	fmov	d[0-9]+, d[0-9]+
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl1_p8, poly8x8_t, poly8x8_t, uint8x8_t)

/*
** test_vtbl1_mf8:
**	fmov	d[0-9]+, d[0-9]+
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl1_mf8, mfloat8x8_t, mfloat8x8_t, uint8x8_t)

/*
** test_vtbl2_s8:
**	sub	sp, sp, #16
**	stp	d[0-9]+, d[0-9]+, \[sp\]
**	ldr	q[0-9]+, \[sp\]
**	add	sp, sp, 16
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl2_s8, int8x8_t, int8x8x2_t, int8x8_t)

/*
** test_vtbl2_u8:
**	sub	sp, sp, #16
**	stp	d[0-9]+, d[0-9]+, \[sp\]
**	ldr	q[0-9]+, \[sp\]
**	add	sp, sp, 16
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl2_u8, uint8x8_t, uint8x8x2_t, uint8x8_t)

/*
** test_vtbl2_p8:
**	sub	sp, sp, #16
**	stp	d[0-9]+, d[0-9]+, \[sp\]
**	ldr	q[0-9]+, \[sp\]
**	add	sp, sp, 16
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl2_p8, poly8x8_t, poly8x8x2_t, uint8x8_t)

/*
** test_vtbl2_mf8:
**	sub	sp, sp, #16
**	stp	d[0-9]+, d[0-9]+, \[sp\]
**	ldr	q[0-9]+, \[sp\]
**	add	sp, sp, 16
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl2_mf8, mfloat8x8_t, mfloat8x8x2_t, uint8x8_t)

/*
** test_vtbl3_s8:
**	zip1	v([0-9]+)\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	fmov	d([0-9]+), d[0-9]+
**	tbl	v[0-9]+\.8b, {v\1\.16b - v\2\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl3_s8, int8x8_t, int8x8x3_t, int8x8_t)

/*
** test_vtbl3_u8:
**	zip1	v([0-9]+)\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	fmov	d([0-9]+), d[0-9]+
**	tbl	v[0-9]+\.8b, {v\1\.16b - v\2\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl3_u8, uint8x8_t, uint8x8x3_t, uint8x8_t)

/*
** test_vtbl3_p8:
**	zip1	v([0-9]+)\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	fmov	d([0-9]+), d[0-9]+
**	tbl	v[0-9]+\.8b, {v\1\.16b - v\2\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl3_p8, poly8x8_t, poly8x8x3_t, uint8x8_t)

/*
** test_vtbl3_mf8:
**	zip1	v([0-9]+)\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	fmov	d([0-9]+), d[0-9]+
**	tbl	v[0-9]+\.8b, {v\1\.16b - v\2\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl3_mf8, mfloat8x8_t, mfloat8x8x3_t, uint8x8_t)

/*
** test_vtbl4_s8:
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl4_s8, int8x8_t, int8x8x4_t, int8x8_t)

/*
** test_vtbl4_u8:
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl4_u8, uint8x8_t, uint8x8x4_t, uint8x8_t)

/*
** test_vtbl4_p8:
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl4_p8, poly8x8_t, poly8x8x4_t, uint8x8_t)

/*
** test_vtbl4_mf8:
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	zip1	v[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vtbl4_mf8, mfloat8x8_t, mfloat8x8x4_t, uint8x8_t)

/*
** test_vqtbl1_s8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl1_s8, int8x8_t, int8x16_t, uint8x8_t)

/*
** test_vqtbl1_u8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl1_u8, uint8x8_t, uint8x16_t, uint8x8_t)

/*
** test_vqtbl1_p8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl1_p8, poly8x8_t, poly8x16_t, uint8x8_t)

/*
** test_vqtbl1_mf8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl1_mf8, mfloat8x8_t, mfloat8x16_t, uint8x8_t)

/*
** test_vqtbl2_s8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl2_s8, int8x8_t, int8x16x2_t, uint8x8_t)

/*
** test_vqtbl2_u8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl2_u8, uint8x8_t, uint8x16x2_t, uint8x8_t)

/*
** test_vqtbl2_p8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl2_p8, poly8x8_t, poly8x16x2_t, uint8x8_t)

/*
** test_vqtbl2_mf8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl2_mf8, mfloat8x8_t, mfloat8x16x2_t, uint8x8_t)

/*
** test_vqtbl3_s8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl3_s8, int8x8_t, int8x16x3_t, uint8x8_t)

/*
** test_vqtbl3_u8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl3_u8, uint8x8_t, uint8x16x3_t, uint8x8_t)

/*
** test_vqtbl3_p8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl3_p8, poly8x8_t, poly8x16x3_t, uint8x8_t)

/*
** test_vqtbl3_mf8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl3_mf8, mfloat8x8_t, mfloat8x16x3_t, uint8x8_t)

/*
** test_vqtbl4_s8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl4_s8, int8x8_t, int8x16x4_t, uint8x8_t)

/*
** test_vqtbl4_u8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl4_u8, uint8x8_t, uint8x16x4_t, uint8x8_t)

/*
** test_vqtbl4_p8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl4_p8, poly8x8_t, poly8x16x4_t, uint8x8_t)

/*
** test_vqtbl4_mf8:
**	tbl	v[0-9]+\.8b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.8b
**	ret
*/
TEST_BINARY (vqtbl4_mf8, mfloat8x8_t, mfloat8x16x4_t, uint8x8_t)

/*
** test_vqtbl1q_s8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl1q_s8, int8x16_t, int8x16_t, uint8x16_t)

/*
** test_vqtbl1q_u8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl1q_u8, uint8x16_t, uint8x16_t, uint8x16_t)

/*
** test_vqtbl1q_p8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl1q_p8, poly8x16_t, poly8x16_t, uint8x16_t)

/*
** test_vqtbl1q_mf8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl1q_mf8, mfloat8x16_t, mfloat8x16_t, uint8x16_t)

/*
** test_vqtbl2q_s8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl2q_s8, int8x16_t, int8x16x2_t, uint8x16_t)

/*
** test_vqtbl2q_u8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl2q_u8, uint8x16_t, uint8x16x2_t, uint8x16_t)

/*
** test_vqtbl2q_p8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl2q_p8, poly8x16_t, poly8x16x2_t, uint8x16_t)

/*
** test_vqtbl2q_mf8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl2q_mf8, mfloat8x16_t, mfloat8x16x2_t, uint8x16_t)

/*
** test_vqtbl3q_s8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl3q_s8, int8x16_t, int8x16x3_t, uint8x16_t)

/*
** test_vqtbl3q_u8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl3q_u8, uint8x16_t, uint8x16x3_t, uint8x16_t)

/*
** test_vqtbl3q_p8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl3q_p8, poly8x16_t, poly8x16x3_t, uint8x16_t)

/*
** test_vqtbl3q_mf8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl3q_mf8, mfloat8x16_t, mfloat8x16x3_t, uint8x16_t)

/*
** test_vqtbl4q_s8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl4q_s8, int8x16_t, int8x16x4_t, uint8x16_t)

/*
** test_vqtbl4q_u8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl4q_u8, uint8x16_t, uint8x16x4_t, uint8x16_t)

/*
** test_vqtbl4q_p8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl4q_p8, poly8x16_t, poly8x16x4_t, uint8x16_t)

/*
** test_vqtbl4q_mf8:
**	tbl	v[0-9]+\.16b, {v[0-9]+\.16b - v[0-9]+\.16b}, v[0-9]+\.16b
**	ret
*/
TEST_BINARY (vqtbl4q_mf8, mfloat8x16_t, mfloat8x16x4_t, uint8x16_t)
