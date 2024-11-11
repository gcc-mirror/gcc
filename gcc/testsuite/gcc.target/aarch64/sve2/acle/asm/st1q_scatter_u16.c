/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** st1q_scatter_u16:
**	st1q	{z0\.q}, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_u16, svuint16_t, svuint64_t,
		       svst1q_scatter_u64base_u16 (p0, z1, z0),
		       svst1q_scatter (p0, z1, z0))

/*
** st1q_scatter_x0_u16_offset:
**	st1q	{z0\.q}, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_x0_u16_offset, svuint16_t, svuint64_t,
		       svst1q_scatter_u64base_offset_u16 (p0, z1, x0, z0),
		       svst1q_scatter_offset (p0, z1, x0, z0))

/*
** st1q_scatter_m1_u16_offset:
**	mov	(x[0-9]+), #?-1
**	st1q	{z0\.q}, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_m1_u16_offset, svuint16_t, svuint64_t,
		       svst1q_scatter_u64base_offset_u16 (p0, z1, -1, z0),
		       svst1q_scatter_offset (p0, z1, -1, z0))

/*
** st1q_scatter_0_u16_offset:
**	st1q	{z0\.q}, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_0_u16_offset, svuint16_t, svuint64_t,
		       svst1q_scatter_u64base_offset_u16 (p0, z1, 0, z0),
		       svst1q_scatter_offset (p0, z1, 0, z0))

/*
** st1q_scatter_1_u16_offset:
**	mov	(x[0-9]+), #?1
**	st1q	{z0\.q}, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1q_scatter_1_u16_offset, svuint16_t, svuint64_t,
		       svst1q_scatter_u64base_offset_u16 (p0, z1, 1, z0),
		       svst1q_scatter_offset (p0, z1, 1, z0))

/*
** st1q_scatter_x0_u16_s64offset:
**	st1q	{z0\.q}, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1q_scatter_x0_u16_s64offset, svuint16_t, uint16_t, svint64_t,
		       svst1q_scatter_s64offset_u16 (p0, x0, z1, z0),
		       svst1q_scatter_offset (p0, x0, z1, z0))

/*
** st1q_scatter_ext_u16_s64offset:
**	sxtw	z1\.d, p0/m, z1\.d
**	st1q	{z0\.q}, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1q_scatter_ext_u16_s64offset, svuint16_t, uint16_t, svint64_t,
		       svst1q_scatter_s64offset_u16 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1q_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1q_scatter_x0_u16_u64offset:
**	st1q	{z0\.q}, p0, \[z1\.d. x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1q_scatter_x0_u16_u64offset, svuint16_t, uint16_t, svuint64_t,
		       svst1q_scatter_u64offset_u16 (p0, x0, z1, z0),
		       svst1q_scatter_offset (p0, x0, z1, z0))

/*
** st1q_scatter_ext_u16_u64offset:
**	and	z1\.d, z1\.d, #0xffffffff
**	st1q	{z0\.q}, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1q_scatter_ext_u16_u64offset, svuint16_t, uint16_t, svuint64_t,
		       svst1q_scatter_u64offset_u16 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1q_scatter_offset (p0, x0, svextw_x (p0, z1), z0))
