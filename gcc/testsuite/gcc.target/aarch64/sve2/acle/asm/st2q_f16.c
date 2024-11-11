/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** st2q_f16_base:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_f16_base, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0, z0),
	    svst2q (p0, x0, z0))

/*
** st2q_f16_index:
**	add	(x[0-9]), x0, x1, lsl #?1
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2q_f16_index, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 + x1, z0),
	    svst2q (p0, x0 + x1, z0))

/*
** st2q_f16_index2:
**	add	(x[0-9]), x0, x1, lsl #?2
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2q_f16_index2, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 + x1 * 2, z0),
	    svst2q (p0, x0 + x1 * 2, z0))

/*
** st2q_f16_index4:
**	add	(x[0-9]), x0, x1, lsl #?3
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2q_f16_index4, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 + x1 * 4, z0),
	    svst2q (p0, x0 + x1 * 4, z0))

/*
** st2q_f16_index8:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_STORE (st2q_f16_index8, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 + x1 * 8, z0),
	    svst2q (p0, x0 + x1 * 8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_f16_1:
**	incb	x0
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_f16_1, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 + svcnth (), z0),
	    svst2q (p0, x0 + svcnth (), z0))

/*
** st2q_f16_2:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2q_f16_2, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 + svcnth () * 2, z0),
	    svst2q (p0, x0 + svcnth () * 2, z0))

/*
** st2q_f16_14:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2q_f16_14, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 + svcnth () * 14, z0),
	    svst2q (p0, x0 + svcnth () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_f16_16:
**	incb	x0, all, mul #16
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_f16_16, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 + svcnth () * 16, z0),
	    svst2q (p0, x0 + svcnth () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_f16_m1:
**	decb	x0
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_f16_m1, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 - svcnth (), z0),
	    svst2q (p0, x0 - svcnth (), z0))

/*
** st2q_f16_m2:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2q_f16_m2, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 - svcnth () * 2, z0),
	    svst2q (p0, x0 - svcnth () * 2, z0))

/*
** st2q_f16_m16:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2q_f16_m16, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 - svcnth () * 16, z0),
	    svst2q (p0, x0 - svcnth () * 16, z0))

/*
** st2q_f16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2q_f16_m18, svfloat16x2_t, float16_t,
	    svst2q_f16 (p0, x0 - svcnth () * 18, z0),
	    svst2q (p0, x0 - svcnth () * 18, z0))

/*
** st2q_vnum_f16_0:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_vnum_f16_0, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, 0, z0),
	    svst2q_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_vnum_f16_1:
**	incb	x0
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_vnum_f16_1, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, 1, z0),
	    svst2q_vnum (p0, x0, 1, z0))

/*
** st2q_vnum_f16_2:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2q_vnum_f16_2, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, 2, z0),
	    svst2q_vnum (p0, x0, 2, z0))

/*
** st2q_vnum_f16_14:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2q_vnum_f16_14, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, 14, z0),
	    svst2q_vnum (p0, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_vnum_f16_16:
**	incb	x0, all, mul #16
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_vnum_f16_16, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, 16, z0),
	    svst2q_vnum (p0, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2q_vnum_f16_m1:
**	decb	x0
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2q_vnum_f16_m1, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, -1, z0),
	    svst2q_vnum (p0, x0, -1, z0))

/*
** st2q_vnum_f16_m2:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2q_vnum_f16_m2, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, -2, z0),
	    svst2q_vnum (p0, x0, -2, z0))

/*
** st2q_vnum_f16_m16:
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2q_vnum_f16_m16, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, -16, z0),
	    svst2q_vnum (p0, x0, -16, z0))

/*
** st2q_vnum_f16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2q	{z0\.q(?: - |, )z1\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2q_vnum_f16_m18, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, -18, z0),
	    svst2q_vnum (p0, x0, -18, z0))

/*
** st2q_vnum_f16_x1:
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
TEST_STORE (st2q_vnum_f16_x1, svfloat16x2_t, float16_t,
	    svst2q_vnum_f16 (p0, x0, x1, z0),
	    svst2q_vnum (p0, x0, x1, z0))
