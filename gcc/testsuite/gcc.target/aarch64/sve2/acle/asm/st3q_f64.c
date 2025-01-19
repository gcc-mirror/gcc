/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** st3q_f64_base:
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_f64_base, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0, z0),
	    svst3q (p0, x0, z0))

/*
** st3q_f64_index:
**	add	(x[0-9]), x0, x1, lsl #?3
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_f64_index, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 + x1, z0),
	    svst3q (p0, x0 + x1, z0))

/*
** st3q_f64_index2:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_STORE (st3q_f64_index2, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 + x1 * 2, z0),
	    svst3q (p0, x0 + x1 * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_f64_1:
**	incb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_f64_1, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 + svcntd (), z0),
	    svst3q (p0, x0 + svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_f64_2:
**	incb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_f64_2, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 + svcntd () * 2, z0),
	    svst3q (p0, x0 + svcntd () * 2, z0))

/*
** st3q_f64_3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3q_f64_3, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 + svcntd () * 3, z0),
	    svst3q (p0, x0 + svcntd () * 3, z0))

/*
** st3q_f64_21:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3q_f64_21, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 + svcntd () * 21, z0),
	    svst3q (p0, x0 + svcntd () * 21, z0))

/*
** st3q_f64_24:
**	addvl	(x[0-9]+), x0, #24
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_f64_24, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 + svcntd () * 24, z0),
	    svst3q (p0, x0 + svcntd () * 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_f64_m1:
**	decb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_f64_m1, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 - svcntd (), z0),
	    svst3q (p0, x0 - svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_f64_m2:
**	decb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_f64_m2, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 - svcntd () * 2, z0),
	    svst3q (p0, x0 - svcntd () * 2, z0))

/*
** st3q_f64_m3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3q_f64_m3, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 - svcntd () * 3, z0),
	    svst3q (p0, x0 - svcntd () * 3, z0))

/*
** st3q_f64_m24:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3q_f64_m24, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 - svcntd () * 24, z0),
	    svst3q (p0, x0 - svcntd () * 24, z0))

/*
** st3q_f64_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_f64_m27, svfloat64x3_t, float64_t,
	    svst3q_f64 (p0, x0 - svcntd () * 27, z0),
	    svst3q (p0, x0 - svcntd () * 27, z0))

/*
** st3q_vnum_f64_0:
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_0, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, 0, z0),
	    svst3q_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_f64_1:
**	incb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_1, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, 1, z0),
	    svst3q_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_f64_2:
**	incb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_2, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, 2, z0),
	    svst3q_vnum (p0, x0, 2, z0))

/*
** st3q_vnum_f64_3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_3, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, 3, z0),
	    svst3q_vnum (p0, x0, 3, z0))

/*
** st3q_vnum_f64_21:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_21, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, 21, z0),
	    svst3q_vnum (p0, x0, 21, z0))

/*
** st3q_vnum_f64_24:
**	addvl	(x[0-9]+), x0, #24
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_24, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, 24, z0),
	    svst3q_vnum (p0, x0, 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_f64_m1:
**	decb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_m1, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, -1, z0),
	    svst3q_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_f64_m2:
**	decb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_m2, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, -2, z0),
	    svst3q_vnum (p0, x0, -2, z0))

/*
** st3q_vnum_f64_m3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_m3, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, -3, z0),
	    svst3q_vnum (p0, x0, -3, z0))

/*
** st3q_vnum_f64_m24:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_m24, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, -24, z0),
	    svst3q_vnum (p0, x0, -24, z0))

/*
** st3q_vnum_f64_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_vnum_f64_m27, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, -27, z0),
	    svst3q_vnum (p0, x0, -27, z0))

/*
** st3q_vnum_f64_x1:
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
TEST_STORE (st3q_vnum_f64_x1, svfloat64x3_t, float64_t,
	    svst3q_vnum_f64 (p0, x0, x1, z0),
	    svst3q_vnum (p0, x0, x1, z0))
