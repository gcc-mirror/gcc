/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#define NO_SHARED_ZA
#include "test_sme_acle.h"

#pragma GCC target "+nosme"

/*
** test_nosme:
**	...
**	bl	__arm_sme_state
**	lsr	x0, x0, #?63
**	...
*/
PROTO (test_nosme, int, ()) { return __arm_has_sme (); }

#pragma GCC target "+sme"

/*
** test_sme:
**	mov	w0, #?1
**	ret
*/
PROTO (test_sme, int, ()) { return __arm_has_sme (); }
