/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#define NO_SHARED_ZA
#include "test_sme_acle.h"

/*
** cntsh:
**	rdsvl	(x[0-9])+, #1
**	lsr	x0, \1, #?1
**	ret
*/
PROTO (cntsh, uint64_t, ()) { return svcntsh (); }
