/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** whilegt_p1_rr_s64:
**	whilegt	{p[0-9]+\.b, p[0-9]+\.b}, x0, x1
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p1_rr_s64, int64_t,
		   p1 = svwhilegt_b8_s64_x2 (x0, x1),
		   p1 = svwhilegt_b8_x2 (x0, x1))

/*
** whilegt_p4_rr_s64:
**	whilegt	{p4\.b, p5\.b}, x0, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p4_rr_s64, int64_t,
		   p4 = svwhilegt_b8_s64_x2 (x0, x1),
		   p4 = svwhilegt_b8_x2 (x0, x1))

/*
** whilegt_p9_rr_s64:
**	whilegt	{p[0-9]+\.b, p[0-9]+\.b}, x0, x1
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p9_rr_s64, int64_t,
		   p9 = svwhilegt_b8_s64_x2 (x0, x1),
		   p9 = svwhilegt_b8_x2 (x0, x1))

/*
** whilegt_p14_rr_s64:
**	whilegt	{p14\.b, p15\.b}, x0, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p14_rr_s64, int64_t,
		   p14 = svwhilegt_b8_s64_x2 (x0, x1),
		   p14 = svwhilegt_b8_x2 (x0, x1))

/*
** whilegt_p4_0r_s64:
**	whilegt	{p4\.b, p5\.b}, xzr, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p4_0r_s64, int64_t,
		   p4 = svwhilegt_b8_x2 ((int64_t) 0, x1),
		   p4 = svwhilegt_b8_s64_x2 (0, x1))

/*
** whilegt_p4_5r_s64:
**	mov	(x[0-9]+), #?5
**	whilegt	{p4\.b, p5\.b}, \1, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p4_5r_s64, int64_t,
		   p4 = svwhilegt_b8_x2 ((int64_t) 5, x1),
		   p4 = svwhilegt_b8_s64_x2 (5, x1))

/*
** whilegt_p4_r0_s64:
**	whilegt	{p4\.b, p5\.b}, x0, xzr
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p4_r0_s64, int64_t,
		   p4 = svwhilegt_b8_x2 (x0, (int64_t) 0),
		   p4 = svwhilegt_b8_s64_x2 (x0, 0))

/*
** whilegt_p14_r5_s64:
**	mov	(x[0-9]+), #?5
**	whilegt	{p14\.b, p15\.b}, x0, \1
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p14_r5_s64, int64_t,
		   p14 = svwhilegt_b8_x2 (x0, (int64_t) 5),
		   p14 = svwhilegt_b8_s64_x2 (x0, 5))

/*
** whilegt_p4_rr_u64:
**	whilehi	{p4\.b, p5\.b}, x0, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p4_rr_u64, uint64_t,
		   p4 = svwhilegt_b8_u64_x2 (x0, x1),
		   p4 = svwhilegt_b8_x2 (x0, x1))

/*
** whilegt_p4_0r_u64:
**	whilehi	{p4\.b, p5\.b}, xzr, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p4_0r_u64, uint64_t,
		   p4 = svwhilegt_b8_x2 ((uint64_t) 0, x1),
		   p4 = svwhilegt_b8_u64_x2 (0, x1))

/*
** whilegt_p4_5r_u64:
**	mov	(x[0-9]+), #?5
**	whilehi	{p4\.b, p5\.b}, \1, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p4_5r_u64, uint64_t,
		   p4 = svwhilegt_b8_x2 ((uint64_t) 5, x1),
		   p4 = svwhilegt_b8_u64_x2 (5, x1))

/*
** whilegt_p4_r5_u64:
**	mov	(x[0-9]+), #?5
**	whilehi	{p4\.b, p5\.b}, x0, \1
**	ret
*/
TEST_COMPARE_S_X2 (whilegt_p4_r5_u64, uint64_t,
		   p4 = svwhilegt_b8_x2 (x0, (uint64_t) 5),
		   p4 = svwhilegt_b8_u64_x2 (x0, 5))
