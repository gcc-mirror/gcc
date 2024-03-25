/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** stnt1_f64_base:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_base, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0, z0),
		  svstnt1 (pn8, x0, z0))

/*
** stnt1_f64_index:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, x1, lsl #?3\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_index, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0 + x1, z0),
		  svstnt1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f64_1:
**	incb	x0
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_1, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0 + svcntd (), z0),
		  svstnt1 (pn8, x0 + svcntd (), z0))

/*
** stnt1_f64_2:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_2, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0 + svcntd () * 2, z0),
		  svstnt1 (pn8, x0 + svcntd () * 2, z0))

/*
** stnt1_f64_14:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_14, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0 + svcntd () * 14, z0),
		  svstnt1 (pn8, x0 + svcntd () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f64_16:
**	incb	x0, all, mul #16
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_16, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0 + svcntd () * 16, z0),
		  svstnt1 (pn8, x0 + svcntd () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f64_m1:
**	decb	x0
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_m1, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0 - svcntd (), z0),
		  svstnt1 (pn8, x0 - svcntd (), z0))

/*
** stnt1_f64_m2:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_m2, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0 - svcntd () * 2, z0),
		  svstnt1 (pn8, x0 - svcntd () * 2, z0))

/*
** stnt1_f64_m16:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_m16, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0 - svcntd () * 16, z0),
		  svstnt1 (pn8, x0 - svcntd () * 16, z0))

/*
** stnt1_f64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_m18, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0 - svcntd () * 18, z0),
		  svstnt1 (pn8, x0 - svcntd () * 18, z0))

/*
** stnt1_f64_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1d	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_z17, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0, z17),
		  svstnt1 (pn8, x0, z17))

/*
** stnt1_f64_z22:
**	stnt1d	{z22\.d(?: - |, )z23\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_z22, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0, z22),
		  svstnt1 (pn8, x0, z22))

/*
** stnt1_f64_z28:
**	stnt1d	{z28\.d(?: - |, )z29\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_z28, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn8, x0, z28),
		  svstnt1 (pn8, x0, z28))

/*
** stnt1_f64_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_pn0, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn0, x0, z0),
		  svstnt1 (pn0, x0, z0))

/*
** stnt1_f64_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_pn7, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn7, x0, z0),
		  svstnt1 (pn7, x0, z0))

/*
** stnt1_f64_pn15:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f64_pn15, svfloat64x2_t, float64_t,
		  svstnt1_f64_x2 (pn15, x0, z0),
		  svstnt1 (pn15, x0, z0))

/*
** stnt1_vnum_f64_0:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_0, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, 0, z0),
		  svstnt1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f64_1:
**	incb	x0
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_1, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, 1, z0),
		  svstnt1_vnum (pn8, x0, 1, z0))

/*
** stnt1_vnum_f64_2:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_2, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, 2, z0),
		  svstnt1_vnum (pn8, x0, 2, z0))

/*
** stnt1_vnum_f64_14:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_14, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, 14, z0),
		  svstnt1_vnum (pn8, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f64_16:
**	incb	x0, all, mul #16
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_16, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, 16, z0),
		  svstnt1_vnum (pn8, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f64_m1:
**	decb	x0
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_m1, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, -1, z0),
		  svstnt1_vnum (pn8, x0, -1, z0))

/*
** stnt1_vnum_f64_m2:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_m2, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, -2, z0),
		  svstnt1_vnum (pn8, x0, -2, z0))

/*
** stnt1_vnum_f64_m16:
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_m16, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, -16, z0),
		  svstnt1_vnum (pn8, x0, -16, z0))

/*
** stnt1_vnum_f64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_m18, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, -18, z0),
		  svstnt1_vnum (pn8, x0, -18, z0))

/*
** stnt1_vnum_f64_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	stnt1d	{z0\.d(?: - |, )z1\.d}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f64_x1, svfloat64x2_t, float64_t,
		  svstnt1_vnum_f64_x2 (pn8, x0, x1, z0),
		  svstnt1_vnum (pn8, x0, x1, z0))
