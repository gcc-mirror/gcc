/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** whilelt_pn0_rr_2_s64:
**	whilelt	pn[0-9]+\.h, x0, x1, vlx2
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn0_rr_2_s64, int64_t,
		  pn0 = svwhilelt_c16_s64 (x0, x1, 2),
		  pn0 = svwhilelt_c16 (x0, x1, 2))

/*
** whilelt_pn7_rr_4_s64:
**	whilelt	pn[0-9]+\.h, x0, x1, vlx4
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn7_rr_4_s64, int64_t,
		  pn7 = svwhilelt_c16_s64 (x0, x1, 4),
		  pn7 = svwhilelt_c16 (x0, x1, 4))

/*
** whilelt_pn8_rr_2_s64:
**	whilelt	pn8\.h, x0, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn8_rr_2_s64, int64_t,
		  pn8 = svwhilelt_c16_s64 (x0, x1, 2),
		  pn8 = svwhilelt_c16 (x0, x1, 2))

/*
** whilelt_pn15_rr_4_s64:
**	whilelt	pn15\.h, x0, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn15_rr_4_s64, int64_t,
		  pn15 = svwhilelt_c16_s64 (x0, x1, 4),
		  pn15 = svwhilelt_c16 (x0, x1, 4))

/*
** whilelt_pn8_0r_2_s64:
**	whilelt	pn8\.h, xzr, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn8_0r_2_s64, int64_t,
		  pn8 = svwhilelt_c16 ((int64_t) 0, x1, 2),
		  pn8 = svwhilelt_c16_s64 (0, x1, 2))

/*
** whilelt_pn8_5r_4_s64:
**	mov	(x[0-9]+), #?5
**	whilelt	pn8\.h, \1, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn8_5r_4_s64, int64_t,
		  pn8 = svwhilelt_c16 ((int64_t) 5, x1, 4),
		  pn8 = svwhilelt_c16_s64 (5, x1, 4))

/*
** whilelt_pn8_r0_2_s64:
**	whilelt	pn8\.h, x0, xzr, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn8_r0_2_s64, int64_t,
		  pn8 = svwhilelt_c16 (x0, (int64_t) 0, 2),
		  pn8 = svwhilelt_c16_s64 (x0, 0, 2))

/*
** whilelt_pn15_r5_4_s64:
**	mov	(x[0-9]+), #?5
**	whilelt	pn15\.h, x0, \1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn15_r5_4_s64, int64_t,
		  pn15 = svwhilelt_c16 (x0, (int64_t) 5, 4),
		  pn15 = svwhilelt_c16_s64 (x0, 5, 4))

/*
** whilelt_pn8_rr_2_u64:
**	whilelo	pn8\.h, x0, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn8_rr_2_u64, uint64_t,
		  pn8 = svwhilelt_c16_u64 (x0, x1, 2),
		  pn8 = svwhilelt_c16 (x0, x1, 2))

/*
** whilelt_pn8_0r_4_u64:
**	whilelo	pn8\.h, xzr, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn8_0r_4_u64, uint64_t,
		  pn8 = svwhilelt_c16 ((uint64_t) 0, x1, 4),
		  pn8 = svwhilelt_c16_u64 (0, x1, 4))

/*
** whilelt_pn8_5r_2_u64:
**	mov	(x[0-9]+), #?5
**	whilelo	pn8\.h, \1, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn8_5r_2_u64, uint64_t,
		  pn8 = svwhilelt_c16 ((uint64_t) 5, x1, 2),
		  pn8 = svwhilelt_c16_u64 (5, x1, 2))

/*
** whilelt_pn8_r5_4_u64:
**	mov	(x[0-9]+), #?5
**	whilelo	pn8\.h, x0, \1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilelt_pn8_r5_4_u64, uint64_t,
		  pn8 = svwhilelt_c16 (x0, (uint64_t) 5, 4),
		  pn8 = svwhilelt_c16_u64 (x0, 5, 4))
