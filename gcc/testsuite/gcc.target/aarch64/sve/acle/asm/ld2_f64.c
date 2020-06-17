/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld2_f64_base:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_f64_base, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0),
	   z0 = svld2 (p0, x0))

/*
** ld2_f64_index:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0, x1, lsl 3\]
**	ret
*/
TEST_LOAD (ld2_f64_index, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0 + x1),
	   z0 = svld2 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_f64_1:
**	incb	x0
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_f64_1, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0 + svcntd ()),
	   z0 = svld2 (p0, x0 + svcntd ()))

/*
** ld2_f64_2:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD (ld2_f64_2, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0 + svcntd () * 2),
	   z0 = svld2 (p0, x0 + svcntd () * 2))

/*
** ld2_f64_14:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD (ld2_f64_14, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0 + svcntd () * 14),
	   z0 = svld2 (p0, x0 + svcntd () * 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_f64_16:
**	incb	x0, all, mul #16
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_f64_16, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0 + svcntd () * 16),
	   z0 = svld2 (p0, x0 + svcntd () * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_f64_m1:
**	decb	x0
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_f64_m1, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0 - svcntd ()),
	   z0 = svld2 (p0, x0 - svcntd ()))

/*
** ld2_f64_m2:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD (ld2_f64_m2, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0 - svcntd () * 2),
	   z0 = svld2 (p0, x0 - svcntd () * 2))

/*
** ld2_f64_m16:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD (ld2_f64_m16, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0 - svcntd () * 16),
	   z0 = svld2 (p0, x0 - svcntd () * 16))

/*
** ld2_f64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2_f64_m18, svfloat64x2_t, float64_t,
	   z0 = svld2_f64 (p0, x0 - svcntd () * 18),
	   z0 = svld2 (p0, x0 - svcntd () * 18))

/*
** ld2_vnum_f64_0:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_0, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, 0),
	   z0 = svld2_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_vnum_f64_1:
**	incb	x0
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_1, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, 1),
	   z0 = svld2_vnum (p0, x0, 1))

/*
** ld2_vnum_f64_2:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_2, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, 2),
	   z0 = svld2_vnum (p0, x0, 2))

/*
** ld2_vnum_f64_14:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_14, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, 14),
	   z0 = svld2_vnum (p0, x0, 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_vnum_f64_16:
**	incb	x0, all, mul #16
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_16, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, 16),
	   z0 = svld2_vnum (p0, x0, 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_vnum_f64_m1:
**	decb	x0
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_m1, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, -1),
	   z0 = svld2_vnum (p0, x0, -1))

/*
** ld2_vnum_f64_m2:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_m2, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, -2),
	   z0 = svld2_vnum (p0, x0, -2))

/*
** ld2_vnum_f64_m16:
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_m16, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, -16),
	   z0 = svld2_vnum (p0, x0, -16))

/*
** ld2_vnum_f64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_m18, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, -18),
	   z0 = svld2_vnum (p0, x0, -18))

/* Using MUL to calculate an index would also be OK.  */
/*
** ld2_vnum_f64_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ld2d	{z0\.d(?: - |, )z1\.d}, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ld2_vnum_f64_x1, svfloat64x2_t, float64_t,
	   z0 = svld2_vnum_f64 (p0, x0, x1),
	   z0 = svld2_vnum (p0, x0, x1))
