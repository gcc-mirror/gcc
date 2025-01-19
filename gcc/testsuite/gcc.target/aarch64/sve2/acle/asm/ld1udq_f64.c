/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** ld1udq_f64_base:
**	ld1d	{z0\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1udq_f64_base, svfloat64_t, float64_t,
	   z0 = svld1udq_f64 (p0, x0),
	   z0 = svld1udq (p0, x0))

/*
** ld1udq_f64_index:
**	ld1d	{z0\.q}, p0/z, \[x0, x1, lsl 3\]
**	ret
*/
TEST_LOAD (ld1udq_f64_index, svfloat64_t, float64_t,
	   z0 = svld1udq_f64 (p0, x0 + x1),
	   z0 = svld1udq (p0, x0 + x1))

/*
** ld1udq_f64_1:
**	ld1d	{z0\.q}, p0/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD (ld1udq_f64_1, svfloat64_t, float64_t,
	   z0 = svld1udq_f64 (p0, x0 + svcntd ()),
	   z0 = svld1udq (p0, x0 + svcntd ()))

/*
** ld1udq_f64_7:
**	ld1d	{z0\.q}, p0/z, \[x0, #6, mul vl\]
**	ret
*/
TEST_LOAD (ld1udq_f64_7, svfloat64_t, float64_t,
	   z0 = svld1udq_f64 (p0, x0 + svcntd () * 3),
	   z0 = svld1udq (p0, x0 + svcntd () * 3))

/* Moving the constant into a register would also be OK.  */
/*
** ld1udq_f64_8:
**	incb	x0, all, mul #4
**	ld1d	{z0\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1udq_f64_8, svfloat64_t, float64_t,
	   z0 = svld1udq_f64 (p0, x0 + svcntd () * 4),
	   z0 = svld1udq (p0, x0 + svcntd () * 4))

/*
** ld1udq_f64_m2:
**	ld1d	{z0\.q}, p0/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD (ld1udq_f64_m2, svfloat64_t, float64_t,
	   z0 = svld1udq_f64 (p0, x0 - svcntd ()),
	   z0 = svld1udq (p0, x0 - svcntd ()))

/*
** ld1udq_f64_m8:
**	ld1d	{z0\.q}, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1udq_f64_m8, svfloat64_t, float64_t,
	   z0 = svld1udq_f64 (p0, x0 - svcntd () * 4),
	   z0 = svld1udq (p0, x0 - svcntd () * 4))

/* Moving the constant into a register would also be OK.  */
/*
** ld1udq_f64_m10:
**	decb	x0, all, mul #5
**	ld1d	{z0\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1udq_f64_m10, svfloat64_t, float64_t,
	   z0 = svld1udq_f64 (p0, x0 - svcntd () * 5),
	   z0 = svld1udq (p0, x0 - svcntd () * 5))

/*
** ld1udq_vnum_f64_0:
**	ld1d	{z0\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1udq_vnum_f64_0, svfloat64_t, float64_t,
	   z0 = svld1udq_vnum_f64 (p0, x0, 0),
	   z0 = svld1udq_vnum (p0, x0, 0))

/*
** ld1udq_vnum_f64_1:
**	ld1d	{z0\.q}, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ld1udq_vnum_f64_1, svfloat64_t, float64_t,
	   z0 = svld1udq_vnum_f64 (p0, x0, 1),
	   z0 = svld1udq_vnum (p0, x0, 1))

/*
** ld1udq_vnum_f64_7:
**	ld1d	{z0\.q}, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ld1udq_vnum_f64_7, svfloat64_t, float64_t,
	   z0 = svld1udq_vnum_f64 (p0, x0, 7),
	   z0 = svld1udq_vnum (p0, x0, 7))

/* Moving the constant into a register would also be OK.  */
/*
** ld1udq_vnum_f64_8:
**	incb	x0, all, mul #4
**	ld1d	{z0\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1udq_vnum_f64_8, svfloat64_t, float64_t,
	   z0 = svld1udq_vnum_f64 (p0, x0, 8),
	   z0 = svld1udq_vnum (p0, x0, 8))

/*
** ld1udq_vnum_f64_m1:
**	ld1d	{z0\.q}, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ld1udq_vnum_f64_m1, svfloat64_t, float64_t,
	   z0 = svld1udq_vnum_f64 (p0, x0, -1),
	   z0 = svld1udq_vnum (p0, x0, -1))

/*
** ld1udq_vnum_f64_m8:
**	ld1d	{z0\.q}, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1udq_vnum_f64_m8, svfloat64_t, float64_t,
	   z0 = svld1udq_vnum_f64 (p0, x0, -8),
	   z0 = svld1udq_vnum (p0, x0, -8))

/* Moving the constant into a register would also be OK.  */
/*
** ld1udq_vnum_f64_m9:
**	dech	x0, all, mul #9
**	ld1d	{z0\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1udq_vnum_f64_m9, svfloat64_t, float64_t,
	   z0 = svld1udq_vnum_f64 (p0, x0, -9),
	   z0 = svld1udq_vnum (p0, x0, -9))

/* Using MUL to calculate an index would also be OK.  */
/*
** ld1udq_vnum_f64_x1:
**	cnth	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ld1d	{z0\.q}, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ld1udq_vnum_f64_x1, svfloat64_t, float64_t,
	   z0 = svld1udq_vnum_f64 (p0, x0, x1),
	   z0 = svld1udq_vnum (p0, x0, x1))
