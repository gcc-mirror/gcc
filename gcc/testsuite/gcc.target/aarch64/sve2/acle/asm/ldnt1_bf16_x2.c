/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** ldnt1_bf16_base:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_base, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0),
		 z0 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_bf16_index:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, x1, lsl #?1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_index, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0 + x1),
		 z0 = svldnt1_x2 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_bf16_1:
**	incb	x0
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_1, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0 + svcnth ()),
		 z0 = svldnt1_x2 (pn8, x0 + svcnth ()))

/*
** ldnt1_bf16_2:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_2, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0 + svcnth () * 2),
		 z0 = svldnt1_x2 (pn8, x0 + svcnth () * 2))

/*
** ldnt1_bf16_14:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_14, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0 + svcnth () * 14),
		 z0 = svldnt1_x2 (pn8, x0 + svcnth () * 14))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_bf16_16:
**	incb	x0, all, mul #16
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_16, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0 + svcnth () * 16),
		 z0 = svldnt1_x2 (pn8, x0 + svcnth () * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_bf16_m1:
**	decb	x0
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_m1, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0 - svcnth ()),
		 z0 = svldnt1_x2 (pn8, x0 - svcnth ()))

/*
** ldnt1_bf16_m2:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_m2, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0 - svcnth () * 2),
		 z0 = svldnt1_x2 (pn8, x0 - svcnth () * 2))

/*
** ldnt1_bf16_m16:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_m16, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0 - svcnth () * 16),
		 z0 = svldnt1_x2 (pn8, x0 - svcnth () * 16))

/*
** ldnt1_bf16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_m18, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn8, x0 - svcnth () * 18),
		 z0 = svldnt1_x2 (pn8, x0 - svcnth () * 18))

/*
** ldnt1_bf16_z17:
**	ldnt1h	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_z17, svbfloat16x2_t, bfloat16_t,
		 z17 = svldnt1_bf16_x2 (pn8, x0),
		 z17 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_bf16_z22:
**	ldnt1h	{z22\.h(?: - |, )z23\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_z22, svbfloat16x2_t, bfloat16_t,
		 z22 = svldnt1_bf16_x2 (pn8, x0),
		 z22 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_bf16_z28:
**	ldnt1h	{z28\.h(?: - |, )z29\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_z28, svbfloat16x2_t, bfloat16_t,
		 z28 = svldnt1_bf16_x2 (pn8, x0),
		 z28 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_bf16_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_pn0, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn0, x0),
		 z0 = svldnt1_x2 (pn0, x0))

/*
** ldnt1_bf16_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_pn7, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn7, x0),
		 z0 = svldnt1_x2 (pn7, x0))

/*
** ldnt1_bf16_pn15:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_bf16_pn15, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_bf16_x2 (pn15, x0),
		 z0 = svldnt1_x2 (pn15, x0))

/*
** ldnt1_vnum_bf16_0:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_0, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, 0),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_bf16_1:
**	incb	x0
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_1, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, 1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 1))

/*
** ldnt1_vnum_bf16_2:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_2, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, 2),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 2))

/*
** ldnt1_vnum_bf16_14:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_14, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, 14),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 14))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_bf16_16:
**	incb	x0, all, mul #16
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_16, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, 16),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 16))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_bf16_m1:
**	decb	x0
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_m1, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, -1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -1))

/*
** ldnt1_vnum_bf16_m2:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_m2, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, -2),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -2))

/*
** ldnt1_vnum_bf16_m16:
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_m16, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, -16),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -16))

/*
** ldnt1_vnum_bf16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_m18, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, -18),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -18))

/*
** ldnt1_vnum_bf16_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldnt1h	{z0\.h(?: - |, )z1\.h}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_bf16_x1, svbfloat16x2_t, bfloat16_t,
		 z0 = svldnt1_vnum_bf16_x2 (pn8, x0, x1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, x1))
