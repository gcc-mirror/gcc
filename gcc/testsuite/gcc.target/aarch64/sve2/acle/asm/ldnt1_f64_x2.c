/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** ldnt1_f64_base:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_base, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0),
		 z0 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_f64_index:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, x1, lsl #?3\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_index, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0 + x1),
		 z0 = svldnt1_x2 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_f64_1:
**	incb	x0
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_1, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0 + svcntd ()),
		 z0 = svldnt1_x2 (pn8, x0 + svcntd ()))

/*
** ldnt1_f64_2:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_2, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0 + svcntd () * 2),
		 z0 = svldnt1_x2 (pn8, x0 + svcntd () * 2))

/*
** ldnt1_f64_14:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_14, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0 + svcntd () * 14),
		 z0 = svldnt1_x2 (pn8, x0 + svcntd () * 14))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_f64_16:
**	incb	x0, all, mul #16
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_16, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0 + svcntd () * 16),
		 z0 = svldnt1_x2 (pn8, x0 + svcntd () * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_f64_m1:
**	decb	x0
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_m1, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0 - svcntd ()),
		 z0 = svldnt1_x2 (pn8, x0 - svcntd ()))

/*
** ldnt1_f64_m2:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_m2, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0 - svcntd () * 2),
		 z0 = svldnt1_x2 (pn8, x0 - svcntd () * 2))

/*
** ldnt1_f64_m16:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_m16, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0 - svcntd () * 16),
		 z0 = svldnt1_x2 (pn8, x0 - svcntd () * 16))

/*
** ldnt1_f64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_m18, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn8, x0 - svcntd () * 18),
		 z0 = svldnt1_x2 (pn8, x0 - svcntd () * 18))

/*
** ldnt1_f64_z17:
**	ldnt1d	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_z17, svfloat64x2_t, float64_t,
		 z17 = svldnt1_f64_x2 (pn8, x0),
		 z17 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_f64_z22:
**	ldnt1d	{z22\.d(?: - |, )z23\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_z22, svfloat64x2_t, float64_t,
		 z22 = svldnt1_f64_x2 (pn8, x0),
		 z22 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_f64_z28:
**	ldnt1d	{z28\.d(?: - |, )z29\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_z28, svfloat64x2_t, float64_t,
		 z28 = svldnt1_f64_x2 (pn8, x0),
		 z28 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_f64_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_pn0, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn0, x0),
		 z0 = svldnt1_x2 (pn0, x0))

/*
** ldnt1_f64_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_pn7, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn7, x0),
		 z0 = svldnt1_x2 (pn7, x0))

/*
** ldnt1_f64_pn15:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f64_pn15, svfloat64x2_t, float64_t,
		 z0 = svldnt1_f64_x2 (pn15, x0),
		 z0 = svldnt1_x2 (pn15, x0))

/*
** ldnt1_vnum_f64_0:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_0, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, 0),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_f64_1:
**	incb	x0
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_1, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, 1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 1))

/*
** ldnt1_vnum_f64_2:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_2, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, 2),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 2))

/*
** ldnt1_vnum_f64_14:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_14, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, 14),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 14))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_f64_16:
**	incb	x0, all, mul #16
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_16, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, 16),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 16))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_f64_m1:
**	decb	x0
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_m1, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, -1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -1))

/*
** ldnt1_vnum_f64_m2:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_m2, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, -2),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -2))

/*
** ldnt1_vnum_f64_m16:
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_m16, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, -16),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -16))

/*
** ldnt1_vnum_f64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_m18, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, -18),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -18))

/*
** ldnt1_vnum_f64_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldnt1d	{z0\.d(?: - |, )z1\.d}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f64_x1, svfloat64x2_t, float64_t,
		 z0 = svldnt1_vnum_f64_x2 (pn8, x0, x1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, x1))
