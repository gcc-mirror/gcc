/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** whilele_pn0_rr_2_s64:
**	whilele	pn[0-9]+\.d, x0, x1, vlx2
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_C (whilele_pn0_rr_2_s64, int64_t,
		  pn0 = svwhilele_c64_s64 (x0, x1, 2),
		  pn0 = svwhilele_c64 (x0, x1, 2))

/*
** whilele_pn7_rr_4_s64:
**	whilele	pn[0-9]+\.d, x0, x1, vlx4
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_C (whilele_pn7_rr_4_s64, int64_t,
		  pn7 = svwhilele_c64_s64 (x0, x1, 4),
		  pn7 = svwhilele_c64 (x0, x1, 4))

/*
** whilele_pn8_rr_2_s64:
**	whilele	pn8\.d, x0, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilele_pn8_rr_2_s64, int64_t,
		  pn8 = svwhilele_c64_s64 (x0, x1, 2),
		  pn8 = svwhilele_c64 (x0, x1, 2))

/*
** whilele_pn15_rr_4_s64:
**	whilele	pn15\.d, x0, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilele_pn15_rr_4_s64, int64_t,
		  pn15 = svwhilele_c64_s64 (x0, x1, 4),
		  pn15 = svwhilele_c64 (x0, x1, 4))

/*
** whilele_pn8_0r_2_s64:
**	whilele	pn8\.d, xzr, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilele_pn8_0r_2_s64, int64_t,
		  pn8 = svwhilele_c64 ((int64_t) 0, x1, 2),
		  pn8 = svwhilele_c64_s64 (0, x1, 2))

/*
** whilele_pn8_5r_4_s64:
**	mov	(x[0-9]+), #?5
**	whilele	pn8\.d, \1, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilele_pn8_5r_4_s64, int64_t,
		  pn8 = svwhilele_c64 ((int64_t) 5, x1, 4),
		  pn8 = svwhilele_c64_s64 (5, x1, 4))

/*
** whilele_pn8_r0_2_s64:
**	whilele	pn8\.d, x0, xzr, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilele_pn8_r0_2_s64, int64_t,
		  pn8 = svwhilele_c64 (x0, (int64_t) 0, 2),
		  pn8 = svwhilele_c64_s64 (x0, 0, 2))

/*
** whilele_pn15_r5_4_s64:
**	mov	(x[0-9]+), #?5
**	whilele	pn15\.d, x0, \1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilele_pn15_r5_4_s64, int64_t,
		  pn15 = svwhilele_c64 (x0, (int64_t) 5, 4),
		  pn15 = svwhilele_c64_s64 (x0, 5, 4))

/*
** whilele_pn8_rr_2_u64:
**	whilels	pn8\.d, x0, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilele_pn8_rr_2_u64, uint64_t,
		  pn8 = svwhilele_c64_u64 (x0, x1, 2),
		  pn8 = svwhilele_c64 (x0, x1, 2))

/*
** whilele_pn8_0r_4_u64:
**	whilels	pn8\.d, xzr, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilele_pn8_0r_4_u64, uint64_t,
		  pn8 = svwhilele_c64 ((uint64_t) 0, x1, 4),
		  pn8 = svwhilele_c64_u64 (0, x1, 4))

/*
** whilele_pn8_5r_2_u64:
**	mov	(x[0-9]+), #?5
**	whilels	pn8\.d, \1, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilele_pn8_5r_2_u64, uint64_t,
		  pn8 = svwhilele_c64 ((uint64_t) 5, x1, 2),
		  pn8 = svwhilele_c64_u64 (5, x1, 2))

/*
** whilele_pn8_r5_4_u64:
**	mov	(x[0-9]+), #?5
**	whilels	pn8\.d, x0, \1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilele_pn8_r5_4_u64, uint64_t,
		  pn8 = svwhilele_c64 (x0, (uint64_t) 5, 4),
		  pn8 = svwhilele_c64_u64 (x0, 5, 4))
