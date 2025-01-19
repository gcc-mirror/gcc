/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** cntp_x0_pn0_2:
**	cntp	x0, pn0\.h, vlx2
**	ret
*/
TEST_COUNT_PN (cntp_x0_pn0_2,
	       x0 = svcntp_c16 (pn0, 2),
	       x0 = svcntp_c16 (pn0, 2))

/*
** cntp_x15_pn7_4:
**	cntp	x15, pn7\.h, vlx4
**	ret
*/
TEST_COUNT_PN (cntp_x15_pn7_4,
	       x15 = svcntp_c16 (pn7, 4),
	       x15 = svcntp_c16 (pn7, 4))

/*
** cntp_x17_pn8_2:
**	cntp	x17, pn8\.h, vlx2
**	ret
*/
TEST_COUNT_PN (cntp_x17_pn8_2,
	       x17 = svcntp_c16 (pn8, 2),
	       x17 = svcntp_c16 (pn8, 2))

/*
** cntp_x0_pn15_4:
**	cntp	x0, pn15\.h, vlx4
**	ret
*/
TEST_COUNT_PN (cntp_x0_pn15_4,
	       x0 = svcntp_c16 (pn15, 4),
	       x0 = svcntp_c16 (pn15, 4))
