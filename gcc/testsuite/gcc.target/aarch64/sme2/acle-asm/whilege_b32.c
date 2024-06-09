/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** whilege_p1_rr_s64:
**	whilege	{p[0-9]+\.s, p[0-9]+\.s}, x0, x1
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p1_rr_s64, int64_t,
		   p1 = svwhilege_b32_s64_x2 (x0, x1),
		   p1 = svwhilege_b32_x2 (x0, x1))

/*
** whilege_p4_rr_s64:
**	whilege	{p4\.s, p5\.s}, x0, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p4_rr_s64, int64_t,
		   p4 = svwhilege_b32_s64_x2 (x0, x1),
		   p4 = svwhilege_b32_x2 (x0, x1))

/*
** whilege_p9_rr_s64:
**	whilege	{p[0-9]+\.s, p[0-9]+\.s}, x0, x1
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p9_rr_s64, int64_t,
		   p9 = svwhilege_b32_s64_x2 (x0, x1),
		   p9 = svwhilege_b32_x2 (x0, x1))

/*
** whilege_p14_rr_s64:
**	whilege	{p14\.s, p15\.s}, x0, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p14_rr_s64, int64_t,
		   p14 = svwhilege_b32_s64_x2 (x0, x1),
		   p14 = svwhilege_b32_x2 (x0, x1))

/*
** whilege_p4_0r_s64:
**	whilege	{p4\.s, p5\.s}, xzr, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p4_0r_s64, int64_t,
		   p4 = svwhilege_b32_x2 ((int64_t) 0, x1),
		   p4 = svwhilege_b32_s64_x2 (0, x1))

/*
** whilege_p4_5r_s64:
**	mov	(x[0-9]+), #?5
**	whilege	{p4\.s, p5\.s}, \1, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p4_5r_s64, int64_t,
		   p4 = svwhilege_b32_x2 ((int64_t) 5, x1),
		   p4 = svwhilege_b32_s64_x2 (5, x1))

/*
** whilege_p4_r0_s64:
**	whilege	{p4\.s, p5\.s}, x0, xzr
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p4_r0_s64, int64_t,
		   p4 = svwhilege_b32_x2 (x0, (int64_t) 0),
		   p4 = svwhilege_b32_s64_x2 (x0, 0))

/*
** whilege_p14_r5_s64:
**	mov	(x[0-9]+), #?5
**	whilege	{p14\.s, p15\.s}, x0, \1
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p14_r5_s64, int64_t,
		   p14 = svwhilege_b32_x2 (x0, (int64_t) 5),
		   p14 = svwhilege_b32_s64_x2 (x0, 5))

/*
** whilege_p4_rr_u64:
**	whilehs	{p4\.s, p5\.s}, x0, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p4_rr_u64, uint64_t,
		   p4 = svwhilege_b32_u64_x2 (x0, x1),
		   p4 = svwhilege_b32_x2 (x0, x1))

/*
** whilege_p4_0r_u64:
**	whilehs	{p4\.s, p5\.s}, xzr, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p4_0r_u64, uint64_t,
		   p4 = svwhilege_b32_x2 ((uint64_t) 0, x1),
		   p4 = svwhilege_b32_u64_x2 (0, x1))

/*
** whilege_p4_5r_u64:
**	mov	(x[0-9]+), #?5
**	whilehs	{p4\.s, p5\.s}, \1, x1
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p4_5r_u64, uint64_t,
		   p4 = svwhilege_b32_x2 ((uint64_t) 5, x1),
		   p4 = svwhilege_b32_u64_x2 (5, x1))

/*
** whilege_p4_r5_u64:
**	mov	(x[0-9]+), #?5
**	whilehs	{p4\.s, p5\.s}, x0, \1
**	ret
*/
TEST_COMPARE_S_X2 (whilege_p4_r5_u64, uint64_t,
		   p4 = svwhilege_b32_x2 (x0, (uint64_t) 5),
		   p4 = svwhilege_b32_u64_x2 (x0, 5))
