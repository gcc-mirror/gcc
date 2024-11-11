/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** st2q_u64_base:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_u64_base, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0, z0),
	    svst2q (p0, x0, z0))

/*
** st2q_u64_index:
**	add	(x[0-9]), x0, x1, lsl #?3
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2q_u64_index, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 + x1, z0),
	    svst2q (p0, x0 + x1, z0))

/*
** st2q_u64_index2:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_STORE (st2q_u64_index2, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 + x1 * 2, z0),
	    svst2q (p0, x0 + x1 * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_u64_1:
**	incb	x0
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_u64_1, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 + svcntd (), z0),
	    svst2q (p0, x0 + svcntd (), z0))

/*
** st2q_u64_2:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2q_u64_2, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 + svcntd () * 2, z0),
	    svst2q (p0, x0 + svcntd () * 2, z0))

/*
** st2q_u64_14:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2q_u64_14, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 + svcntd () * 14, z0),
	    svst2q (p0, x0 + svcntd () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_u64_16:
**	incb	x0, all, mul #16
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_u64_16, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 + svcntd () * 16, z0),
	    svst2q (p0, x0 + svcntd () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_u64_m1:
**	decb	x0
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_u64_m1, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 - svcntd (), z0),
	    svst2q (p0, x0 - svcntd (), z0))

/*
** st2q_u64_m2:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2q_u64_m2, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 - svcntd () * 2, z0),
	    svst2q (p0, x0 - svcntd () * 2, z0))

/*
** st2q_u64_m16:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2q_u64_m16, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 - svcntd () * 16, z0),
	    svst2q (p0, x0 - svcntd () * 16, z0))

/*
** st2q_u64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2q_u64_m18, svuint64x2_t, uint64_t,
	    svst2q_u64 (p0, x0 - svcntd () * 18, z0),
	    svst2q (p0, x0 - svcntd () * 18, z0))

/*
** st2q_vnum_u64_0:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_vnum_u64_0, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, 0, z0),
	    svst2q_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_vnum_u64_1:
**	incb	x0
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_vnum_u64_1, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, 1, z0),
	    svst2q_vnum (p0, x0, 1, z0))

/*
** st2q_vnum_u64_2:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2q_vnum_u64_2, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, 2, z0),
	    svst2q_vnum (p0, x0, 2, z0))

/*
** st2q_vnum_u64_14:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2q_vnum_u64_14, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, 14, z0),
	    svst2q_vnum (p0, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_vnum_u64_16:
**	incb	x0, all, mul #16
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_vnum_u64_16, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, 16, z0),
	    svst2q_vnum (p0, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_vnum_u64_m1:
**	decb	x0
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_vnum_u64_m1, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, -1, z0),
	    svst2q_vnum (p0, x0, -1, z0))

/*
** st2q_vnum_u64_m2:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2q_vnum_u64_m2, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, -2, z0),
	    svst2q_vnum (p0, x0, -2, z0))

/*
** st2q_vnum_u64_m16:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2q_vnum_u64_m16, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, -16, z0),
	    svst2q_vnum (p0, x0, -16, z0))

/*
** st2q_vnum_u64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2q_vnum_u64_m18, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, -18, z0),
	    svst2q_vnum (p0, x0, -18, z0))

/*
** st2q_vnum_u64_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, \3\]
** )
**	ret
*/
TEST_STORE (st2q_vnum_u64_x1, svuint64x2_t, uint64_t,
	    svst2q_vnum_u64 (p0, x0, x1, z0),
	    svst2q_vnum (p0, x0, x1, z0))
