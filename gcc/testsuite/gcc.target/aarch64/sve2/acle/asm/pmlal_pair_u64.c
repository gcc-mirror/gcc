/* { dg-do assemble { target { aarch64_asm_ssve-aes_ok && aarch64_asm_sve-aes2_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_ssve-aes_ok && aarch64_asm_sve-aes2_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve-aes2+ssve-aes"

/*
**test_pmlal_pair_u64:
**	pmlal	{z0.q - z1.q}, z2.d, z2.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_u64, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_u64_x2(t, v, v),
            t = svpmlal_pair(t, v, v))

/*
**test_pmlal_pair_n_u64_regs:
**	pmlal	{z0.q - z1.q}, z4.d, z5.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_n_u64_regs, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_u64_x2(z0, z4, z5),
            t = svpmlal_pair(z0, z4, z5))

/*
**test_pmlal_pair_n_u64_regs_imm:
**	movi	d[0-9]{1,2}, #0
**	pmlal	{z0.q - z1.q}, z4.d, z[0-9]{1,2}\.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_n_u64_regs_imm, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_n_u64_x2(z0, z4, 0x0),
            t = svpmlal_pair(z0, z4, 0x0))

/*
**test_pmlal_pair_n_u64_regs_x0:
**	mov	z([0-9]{1,2})\.d, x0
**	pmlal	{z0.q - z1.q}, z4.d, z\1.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_n_u64_regs_x0, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_n_u64_x2(z0, z4, x0),
            t = svpmlal_pair(z0, z4, x0))

/*
**test_pmlal_pair_n_u64_regs_imm_1:
**	mov	z[0-9]{1,2}\.d, #65535
**	pmlal	{z0.q - z1.q}, z4.d, z[0-9]{1,2}\.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_n_u64_regs_imm_1, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_n_u64_x2(z0, z4, 0xFFFF),
            t = svpmlal_pair(z0, z4, 0xFFFF))

/*
**test_pmlal_pair_n_u64_regs_x0_1:
**	mov	z([0-9]{1,2})\.d, x0
**	pmlal	{z0.q - z1.q}, z4.d, z\1.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_n_u64_regs_x0_1, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_n_u64_x2(z0, z4, x0),
            t = svpmlal_pair(z0, z4, x0))

/*
**test_pmlal_pair_n_u64_regs_mov:
**	movi	d30, #0
**	mov	z0.d, z3.d
**	mov	z1.d, z4.d
**	pmlal	{z0.q - z1.q}, z4.d, z30.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_n_u64_regs_mov, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_n_u64_x2(z3, z4, 0x0),
            t = svpmlal_pair(z3, z4, 0x0))

/*
**test_pmlal_pair_n_u64_regs_mov_x0:
**	mov	z30.d, x0
**	mov	z0.d, z3.d
**	mov	z1.d, z4.d
**	pmlal	{z0.q - z1.q}, z4.d, z30.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_n_u64_regs_mov_x0, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_n_u64_x2(z3, z4, x0),
            t = svpmlal_pair(z3, z4, x0))

/*
**test_pmlal_pair_n_u64_regs_mov_1:
**	mov	z30.d, #65535
**	mov	z0.d, z3.d
**	mov	z1.d, z4.d
**	pmlal	{z0.q - z1.q}, z4.d, z30.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_n_u64_regs_mov_1, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_n_u64_x2(z3, z4, 0xFFFF),
            t = svpmlal_pair(z3, z4, 0xFFFF))

/*
**test_pmlal_pair_n_u64_regs_mov_x0_1:
**	mov	z30.d, x0
**	mov	z0.d, z3.d
**	mov	z1.d, z4.d
**	pmlal	{z0.q - z1.q}, z4.d, z30.d
**	ret
*/
TEST_XN_INDEXED(test_pmlal_pair_n_u64_regs_mov_x0_1, svuint64x2_t, svuint64_t,
            t = svpmlal_pair_n_u64_x2(z3, z4, x0),
            t = svpmlal_pair(z3, z4, x0))
