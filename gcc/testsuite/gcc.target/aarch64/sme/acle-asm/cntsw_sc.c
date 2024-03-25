/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#define NO_SHARED_ZA
#include "test_sme_acle.h"

/*
** cntsw:
**	rdsvl	(x[0-9])+, #1
**	lsr	x0, \1, #?2
**	ret
*/
PROTO (cntsw, uint64_t, ()) { return svcntsw (); }
