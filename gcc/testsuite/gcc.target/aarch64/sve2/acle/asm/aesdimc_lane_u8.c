/* { dg-do assemble { target { aarch64_asm_ssve-aes_ok && aarch64_asm_sve-aes2_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_ssve-aes_ok && aarch64_asm_sve-aes2_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve-aes2+ssve-aes"

/*
** test_aesdimc_lane_u8_x2:
**	aesdimc	{z0.b - z1.b}, {z0.b - z1.b}, z2.q\[0\]
**	ret
*/
TEST_XN_INDEXED (test_aesdimc_lane_u8_x2, svuint8x2_t, svuint8_t,
               t = svaesdimc_lane_u8_x2 (t, v, 0),
               t = svaesdimc_lane (t, v, 0))

/*
** test_aesdimc_lane_u8_x2_lane1:
**	aesdimc	{z0.b - z1.b}, {z0.b - z1.b}, z2.q\[3\]
**	ret
*/
TEST_XN_INDEXED (test_aesdimc_lane_u8_x2_lane1, svuint8x2_t, svuint8_t,
               t = svaesdimc_lane_u8_x2 (t, v, 3),
               t = svaesdimc_lane (t, v, 3))

/*
** test_aesdimc_lane_u8_x4:
**	aesdimc	{z0.b - z3.b}, {z0.b - z3.b}, z4.q\[0\]
**	ret
*/
TEST_XN_INDEXED (test_aesdimc_lane_u8_x4, svuint8x4_t, svuint8_t,
               t = svaesdimc_lane_u8_x4 (t, v, 0),
               t = svaesdimc_lane (t, v, 0))

/*
** test_aesdimc_lane_u8_x4_lane3:
**	aesdimc	{z0.b - z3.b}, {z0.b - z3.b}, z4.q\[3\]
**	ret
*/
TEST_XN_INDEXED (test_aesdimc_lane_u8_x4_lane3, svuint8x4_t, svuint8_t,
               t = svaesdimc_lane_u8_x4 (t, v, 3),
               t = svaesdimc_lane (t, v, 3))

/*
** test_aesdimc_lane_u8_x2_regs_mov:
**	mov	z0.d, z3.d
**	mov	z1.d, z4.d
**	aesdimc	{z0.b - z1.b}, {z0.b - z1.b}, z4.q\[0\]
**	ret
*/
TEST_XN_INDEXED (test_aesdimc_lane_u8_x2_regs_mov, svuint8x2_t, svuint8_t,
               t = svaesdimc_lane_u8_x2 (z3, z4, 0),
               t = svaesdimc_lane (z3, z4, 0))

/*
** test_aesdimc_lane_u8_x2_regs_mov_lane1:
**	mov	z0.d, z3.d
**	mov	z1.d, z4.d
**	aesdimc	{z0.b - z1.b}, {z0.b - z1.b}, z4.q\[3\]
**	ret
*/
TEST_XN_INDEXED (test_aesdimc_lane_u8_x2_regs_mov_lane1, svuint8x2_t, svuint8_t,
               t = svaesdimc_lane_u8_x2 (z3, z4, 3),
               t = svaesdimc_lane (z3, z4, 3))

/*
** test_aesdimc_lane_u8_x4_regs_mov:
**	mov	z0.d, z3.d
**	mov	z1.d, z4.d
**	mov	z2.d, z5.d
**	mov	z3.d, z6.d
**	aesdimc	{z0.b - z3.b}, {z0.b - z3.b}, z4.q\[0\]
**	ret
*/
TEST_XN_INDEXED (test_aesdimc_lane_u8_x4_regs_mov, svuint8x4_t, svuint8_t,
               t = svaesdimc_lane_u8_x4 (z3, z4, 0),
               t = svaesdimc_lane (z3, z4, 0))

/*
** test_aesdimc_lane_u8_x4_regs_mov_lane3:
**	mov	z0.d, z3.d
**	mov	z1.d, z4.d
**	mov	z2.d, z5.d
**	mov	z3.d, z6.d
**	aesdimc	{z0.b - z3.b}, {z0.b - z3.b}, z4.q\[3\]
**	ret
*/
TEST_XN_INDEXED (test_aesdimc_lane_u8_x4_regs_mov_lane3, svuint8x4_t, svuint8_t,
               t = svaesdimc_lane_u8_x4 (z3, z4, 3),
               t = svaesdimc_lane (z3, z4, 3))
