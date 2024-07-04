/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#define NO_SHARED_ZA
#include "test_sme_acle.h"

/*
** cntsb:
**	rdsvl	x0, #1
**	ret
*/
PROTO (cntsb, uint64_t, ()) { return svcntsb (); }
