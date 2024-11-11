/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** whilege_pn0_rr_2_s64:
**	whilege	pn[0-9]+\.b, x0, x1, vlx2
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_C (whilege_pn0_rr_2_s64, int64_t,
		  pn0 = svwhilege_c8_s64 (x0, x1, 2),
		  pn0 = svwhilege_c8 (x0, x1, 2))

/*
** whilege_pn7_rr_4_s64:
**	whilege	pn[0-9]+\.b, x0, x1, vlx4
**	mov	[^\n]+
**	ret
*/
TEST_COMPARE_S_C (whilege_pn7_rr_4_s64, int64_t,
		  pn7 = svwhilege_c8_s64 (x0, x1, 4),
		  pn7 = svwhilege_c8 (x0, x1, 4))

/*
** whilege_pn8_rr_2_s64:
**	whilege	pn8\.b, x0, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilege_pn8_rr_2_s64, int64_t,
		  pn8 = svwhilege_c8_s64 (x0, x1, 2),
		  pn8 = svwhilege_c8 (x0, x1, 2))

/*
** whilege_pn15_rr_4_s64:
**	whilege	pn15\.b, x0, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilege_pn15_rr_4_s64, int64_t,
		  pn15 = svwhilege_c8_s64 (x0, x1, 4),
		  pn15 = svwhilege_c8 (x0, x1, 4))

/*
** whilege_pn8_0r_2_s64:
**	whilege	pn8\.b, xzr, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilege_pn8_0r_2_s64, int64_t,
		  pn8 = svwhilege_c8 ((int64_t) 0, x1, 2),
		  pn8 = svwhilege_c8_s64 (0, x1, 2))

/*
** whilege_pn8_5r_4_s64:
**	mov	(x[0-9]+), #?5
**	whilege	pn8\.b, \1, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilege_pn8_5r_4_s64, int64_t,
		  pn8 = svwhilege_c8 ((int64_t) 5, x1, 4),
		  pn8 = svwhilege_c8_s64 (5, x1, 4))

/*
** whilege_pn8_r0_2_s64:
**	whilege	pn8\.b, x0, xzr, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilege_pn8_r0_2_s64, int64_t,
		  pn8 = svwhilege_c8 (x0, (int64_t) 0, 2),
		  pn8 = svwhilege_c8_s64 (x0, 0, 2))

/*
** whilege_pn15_r5_4_s64:
**	mov	(x[0-9]+), #?5
**	whilege	pn15\.b, x0, \1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilege_pn15_r5_4_s64, int64_t,
		  pn15 = svwhilege_c8 (x0, (int64_t) 5, 4),
		  pn15 = svwhilege_c8_s64 (x0, 5, 4))

/*
** whilege_pn8_rr_2_u64:
**	whilehs	pn8\.b, x0, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilege_pn8_rr_2_u64, uint64_t,
		  pn8 = svwhilege_c8_u64 (x0, x1, 2),
		  pn8 = svwhilege_c8 (x0, x1, 2))

/*
** whilege_pn8_0r_4_u64:
**	whilehs	pn8\.b, xzr, x1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilege_pn8_0r_4_u64, uint64_t,
		  pn8 = svwhilege_c8 ((uint64_t) 0, x1, 4),
		  pn8 = svwhilege_c8_u64 (0, x1, 4))

/*
** whilege_pn8_5r_2_u64:
**	mov	(x[0-9]+), #?5
**	whilehs	pn8\.b, \1, x1, vlx2
**	ret
*/
TEST_COMPARE_S_C (whilege_pn8_5r_2_u64, uint64_t,
		  pn8 = svwhilege_c8 ((uint64_t) 5, x1, 2),
		  pn8 = svwhilege_c8_u64 (5, x1, 2))

/*
** whilege_pn8_r5_4_u64:
**	mov	(x[0-9]+), #?5
**	whilehs	pn8\.b, x0, \1, vlx4
**	ret
*/
TEST_COMPARE_S_C (whilege_pn8_r5_4_u64, uint64_t,
		  pn8 = svwhilege_c8 (x0, (uint64_t) 5, 4),
		  pn8 = svwhilege_c8_u64 (x0, 5, 4))
