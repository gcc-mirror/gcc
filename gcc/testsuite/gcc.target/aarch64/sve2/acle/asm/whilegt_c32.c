/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** whilegt_pn0_rr_2_s64:
**	whilegt	pn[0-9]+\.s, x0, x1, vlx2
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn0_rr_2_s64, int64_t,
		  pn0 = svwhilegt_c32_s64 (x0, x1, 2),
		  pn0 = svwhilegt_c32 (x0, x1, 2))

/*
** whilegt_pn7_rr_4_s64:
**	whilegt	pn[0-9]+\.s, x0, x1, vlx4
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn7_rr_4_s64, int64_t,
		  pn7 = svwhilegt_c32_s64 (x0, x1, 4),
		  pn7 = svwhilegt_c32 (x0, x1, 4))

/*
** whilegt_pn8_rr_2_s64:
**	whilegt	pn8\.s, x0, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn8_rr_2_s64, int64_t,
		  pn8 = svwhilegt_c32_s64 (x0, x1, 2),
		  pn8 = svwhilegt_c32 (x0, x1, 2))

/*
** whilegt_pn15_rr_4_s64:
**	whilegt	pn15\.s, x0, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn15_rr_4_s64, int64_t,
		  pn15 = svwhilegt_c32_s64 (x0, x1, 4),
		  pn15 = svwhilegt_c32 (x0, x1, 4))

/*
** whilegt_pn8_0r_2_s64:
**	whilegt	pn8\.s, xzr, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn8_0r_2_s64, int64_t,
		  pn8 = svwhilegt_c32 ((int64_t) 0, x1, 2),
		  pn8 = svwhilegt_c32_s64 (0, x1, 2))

/*
** whilegt_pn8_5r_4_s64:
**	mov	(x[0-9]+), #?5
**	whilegt	pn8\.s, \1, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn8_5r_4_s64, int64_t,
		  pn8 = svwhilegt_c32 ((int64_t) 5, x1, 4),
		  pn8 = svwhilegt_c32_s64 (5, x1, 4))

/*
** whilegt_pn8_r0_2_s64:
**	whilegt	pn8\.s, x0, xzr, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn8_r0_2_s64, int64_t,
		  pn8 = svwhilegt_c32 (x0, (int64_t) 0, 2),
		  pn8 = svwhilegt_c32_s64 (x0, 0, 2))

/*
** whilegt_pn15_r5_4_s64:
**	mov	(x[0-9]+), #?5
**	whilegt	pn15\.s, x0, \1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn15_r5_4_s64, int64_t,
		  pn15 = svwhilegt_c32 (x0, (int64_t) 5, 4),
		  pn15 = svwhilegt_c32_s64 (x0, 5, 4))

/*
** whilegt_pn8_rr_2_u64:
**	whilehi	pn8\.s, x0, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn8_rr_2_u64, uint64_t,
		  pn8 = svwhilegt_c32_u64 (x0, x1, 2),
		  pn8 = svwhilegt_c32 (x0, x1, 2))

/*
** whilegt_pn8_0r_4_u64:
**	whilehi	pn8\.s, xzr, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn8_0r_4_u64, uint64_t,
		  pn8 = svwhilegt_c32 ((uint64_t) 0, x1, 4),
		  pn8 = svwhilegt_c32_u64 (0, x1, 4))

/*
** whilegt_pn8_5r_2_u64:
**	mov	(x[0-9]+), #?5
**	whilehi	pn8\.s, \1, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn8_5r_2_u64, uint64_t,
		  pn8 = svwhilegt_c32 ((uint64_t) 5, x1, 2),
		  pn8 = svwhilegt_c32_u64 (5, x1, 2))

/*
** whilegt_pn8_r5_4_u64:
**	mov	(x[0-9]+), #?5
**	whilehi	pn8\.s, x0, \1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilegt_pn8_r5_4_u64, uint64_t,
		  pn8 = svwhilegt_c32 (x0, (uint64_t) 5, 4),
		  pn8 = svwhilegt_c32_u64 (x0, 5, 4))
