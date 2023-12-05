/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** cntp_x0_pn0_2:
**	cntp	x0, pn0\.d, vlx2
**	ret
*/
TEST_COUNT_PN (cntp_x0_pn0_2,
	       x0 = svcntp_c64 (pn0, 2),
	       x0 = svcntp_c64 (pn0, 2))

/*
** cntp_x15_pn7_4:
**	cntp	x15, pn7\.d, vlx4
**	ret
*/
TEST_COUNT_PN (cntp_x15_pn7_4,
	       x15 = svcntp_c64 (pn7, 4),
	       x15 = svcntp_c64 (pn7, 4))

/*
** cntp_x17_pn8_2:
**	cntp	x17, pn8\.d, vlx2
**	ret
*/
TEST_COUNT_PN (cntp_x17_pn8_2,
	       x17 = svcntp_c64 (pn8, 2),
	       x17 = svcntp_c64 (pn8, 2))

/*
** cntp_x0_pn15_4:
**	cntp	x0, pn15\.d, vlx4
**	ret
*/
TEST_COUNT_PN (cntp_x0_pn15_4,
	       x0 = svcntp_c64 (pn15, 4),
	       x0 = svcntp_c64 (pn15, 4))
