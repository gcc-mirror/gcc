/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** st1q_scatter_u8:
**	st1q	{z0\.q}, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_u8, svuint8_t, svuint64_t,
		       svst1q_scatter_u64base_u8 (p0, z1, z0),
		       svst1q_scatter (p0, z1, z0))

/*
** st1q_scatter_x0_u8_offset:
**	st1q	{z0\.q}, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_x0_u8_offset, svuint8_t, svuint64_t,
		       svst1q_scatter_u64base_offset_u8 (p0, z1, x0, z0),
		       svst1q_scatter_offset (p0, z1, x0, z0))

/*
** st1q_scatter_m1_u8_offset:
**	mov	(x[0-9]+), #?-1
**	st1q	{z0\.q}, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_m1_u8_offset, svuint8_t, svuint64_t,
		       svst1q_scatter_u64base_offset_u8 (p0, z1, -1, z0),
		       svst1q_scatter_offset (p0, z1, -1, z0))

/*
** st1q_scatter_0_u8_offset:
**	st1q	{z0\.q}, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_0_u8_offset, svuint8_t, svuint64_t,
		       svst1q_scatter_u64base_offset_u8 (p0, z1, 0, z0),
		       svst1q_scatter_offset (p0, z1, 0, z0))

/*
** st1q_scatter_1_u8_offset:
**	mov	(x[0-9]+), #?1
**	st1q	{z0\.q}, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_1_u8_offset, svuint8_t, svuint64_t,
		       svst1q_scatter_u64base_offset_u8 (p0, z1, 1, z0),
		       svst1q_scatter_offset (p0, z1, 1, z0))

/*
** st1q_scatter_x0_u8_s64offset:
**	st1q	{z0\.q}, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1q_scatter_x0_u8_s64offset, svuint8_t, uint8_t, svint64_t,
		       svst1q_scatter_s64offset_u8 (p0, x0, z1, z0),
		       svst1q_scatter_offset (p0, x0, z1, z0))

/*
** st1q_scatter_ext_u8_s64offset:
**	sxtw	z1\.d, p0/m, z1\.d
**	st1q	{z0\.q}, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1q_scatter_ext_u8_s64offset, svuint8_t, uint8_t, svint64_t,
		       svst1q_scatter_s64offset_u8 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1q_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1q_scatter_x0_u8_u64offset:
**	st1q	{z0\.q}, p0, \[z1\.d. x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1q_scatter_x0_u8_u64offset, svuint8_t, uint8_t, svuint64_t,
		       svst1q_scatter_u64offset_u8 (p0, x0, z1, z0),
		       svst1q_scatter_offset (p0, x0, z1, z0))

/*
** st1q_scatter_ext_u8_u64offset:
**	and	z1\.d, z1\.d, #0xffffffff
**	st1q	{z0\.q}, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1q_scatter_ext_u8_u64offset, svuint8_t, uint8_t, svuint64_t,
		       svst1q_scatter_u64offset_u8 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1q_scatter_offset (p0, x0, svextw_x (p0, z1), z0))
