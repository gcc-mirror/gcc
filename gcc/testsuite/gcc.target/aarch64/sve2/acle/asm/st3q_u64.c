/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** st3q_u64_base:
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u64_base, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0, z0),
	    svst3q (p0, x0, z0))

/*
** st3q_u64_index:
**	add	(x[0-9]), x0, x1, lsl #?3
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_u64_index, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 + x1, z0),
	    svst3q (p0, x0 + x1, z0))

/*
** st3q_u64_index2:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_STORE (st3q_u64_index2, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 + x1 * 2, z0),
	    svst3q (p0, x0 + x1 * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_u64_1:
**	incb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u64_1, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 + svcntd (), z0),
	    svst3q (p0, x0 + svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_u64_2:
**	incb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u64_2, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 + svcntd () * 2, z0),
	    svst3q (p0, x0 + svcntd () * 2, z0))

/*
** st3q_u64_3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3q_u64_3, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 + svcntd () * 3, z0),
	    svst3q (p0, x0 + svcntd () * 3, z0))

/*
** st3q_u64_21:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3q_u64_21, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 + svcntd () * 21, z0),
	    svst3q (p0, x0 + svcntd () * 21, z0))

/*
** st3q_u64_24:
**	addvl	(x[0-9]+), x0, #24
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_u64_24, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 + svcntd () * 24, z0),
	    svst3q (p0, x0 + svcntd () * 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_u64_m1:
**	decb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u64_m1, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 - svcntd (), z0),
	    svst3q (p0, x0 - svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_u64_m2:
**	decb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u64_m2, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 - svcntd () * 2, z0),
	    svst3q (p0, x0 - svcntd () * 2, z0))

/*
** st3q_u64_m3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3q_u64_m3, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 - svcntd () * 3, z0),
	    svst3q (p0, x0 - svcntd () * 3, z0))

/*
** st3q_u64_m24:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3q_u64_m24, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 - svcntd () * 24, z0),
	    svst3q (p0, x0 - svcntd () * 24, z0))

/*
** st3q_u64_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_u64_m27, svuint64x3_t, uint64_t,
	    svst3q_u64 (p0, x0 - svcntd () * 27, z0),
	    svst3q (p0, x0 - svcntd () * 27, z0))

/*
** st3q_vnum_u64_0:
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_0, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, 0, z0),
	    svst3q_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_u64_1:
**	incb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_1, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, 1, z0),
	    svst3q_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_u64_2:
**	incb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_2, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, 2, z0),
	    svst3q_vnum (p0, x0, 2, z0))

/*
** st3q_vnum_u64_3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_3, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, 3, z0),
	    svst3q_vnum (p0, x0, 3, z0))

/*
** st3q_vnum_u64_21:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_21, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, 21, z0),
	    svst3q_vnum (p0, x0, 21, z0))

/*
** st3q_vnum_u64_24:
**	addvl	(x[0-9]+), x0, #24
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_24, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, 24, z0),
	    svst3q_vnum (p0, x0, 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_u64_m1:
**	decb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_m1, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, -1, z0),
	    svst3q_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_u64_m2:
**	decb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_m2, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, -2, z0),
	    svst3q_vnum (p0, x0, -2, z0))

/*
** st3q_vnum_u64_m3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_m3, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, -3, z0),
	    svst3q_vnum (p0, x0, -3, z0))

/*
** st3q_vnum_u64_m24:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_m24, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, -24, z0),
	    svst3q_vnum (p0, x0, -24, z0))

/*
** st3q_vnum_u64_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_vnum_u64_m27, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, -27, z0),
	    svst3q_vnum (p0, x0, -27, z0))

/*
** st3q_vnum_u64_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st3q	{z0\.q - z2\.q}, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st3q	{z0\.q - z2\.q}, p0, \[x0, \3\]
** )
**	ret
*/
TEST_STORE (st3q_vnum_u64_x1, svuint64x3_t, uint64_t,
	    svst3q_vnum_u64 (p0, x0, x1, z0),
	    svst3q_vnum (p0, x0, x1, z0))
