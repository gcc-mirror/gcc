/* { dg-do assemble { target { aarch64_asm_ssve-aes_ok && aarch64_asm_sve-aes2_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_ssve-aes_ok && aarch64_asm_sve-aes2_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve-aes2+ssve-aes"

/*
**test_pmull_pair_u64:
**	pmull	{z0.q - z1.q}, z2.d, z2.d
**	ret
*/
TEST_XN_INDEXED(test_pmull_pair_u64, svuint64x2_t, svuint64_t,
            t = svpmull_pair_u64_x2(v, v),
            t = svpmull_pair(v, v))

/*
**test_pmull_pair_n_u64:
**	movi	d([0-9]{1,2}), #0
**	pmull	{z0.q - z1.q}, z2.d, z\1.d
**	ret
*/
TEST_XN_INDEXED(test_pmull_pair_n_u64, svuint64x2_t, svuint64_t,
            t = svpmull_pair_n_u64_x2(v, 0x0),
            t = svpmull_pair(v, 0x0))

/*
**test_pmull_pair_n_u64_x0:
**	mov	z([0-9]{1,2})\.d, x0
**	pmull	{z0.q - z1.q}, z2.d, z\1.d
**	ret
*/
TEST_XN_INDEXED(test_pmull_pair_n_u64_x0, svuint64x2_t, svuint64_t,
            t = svpmull_pair_n_u64_x2(v, x0),
            t = svpmull_pair(v, x0))

/*
**test_pmull_pair_u64_regs:
**	pmull	{z0.q - z1.q}, z4.d, z5.d
**	ret
*/
TEST_XN_INDEXED(test_pmull_pair_u64_regs, svuint64x2_t, svuint64_t,
            t = svpmull_pair_u64_x2(z4, z5),
            t = svpmull_pair(z4, z5))

/*
**test_pmull_pair_n_u64_regs:
**	movi	d([0-9]{1,2}), #0
**	pmull	{z0.q - z1.q}, z4.d, z\1.d
**	ret
*/
TEST_XN_INDEXED(test_pmull_pair_n_u64_regs, svuint64x2_t, svuint64_t,
            t = svpmull_pair_n_u64_x2(z4, 0x0),
            t = svpmull_pair(z4, 0x0))

/*
**test_pmull_pair_n_u64_regs_x0:
**	mov	z([0-9]{1,2})\.d, x0
**	pmull	{z0.q - z1.q}, z4.d, z\1.d
**	ret
*/
TEST_XN_INDEXED(test_pmull_pair_n_u64_regs_x0, svuint64x2_t, svuint64_t,
            t = svpmull_pair_n_u64_x2(z4, x0),
            t = svpmull_pair(z4, x0))
