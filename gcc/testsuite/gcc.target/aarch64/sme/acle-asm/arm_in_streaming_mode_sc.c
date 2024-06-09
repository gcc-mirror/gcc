/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#define NO_SHARED_ZA
#include "test_sme_acle.h"

#pragma GCC target "+nosme"

/*
** test_nosme:
**	...
**	bl	__arm_sme_state
**	and	w0, w0, #?1
**	...
*/
PROTO (test_nosme, int, ()) { return __arm_in_streaming_mode (); }

#pragma GCC target "+sme"

/*
** test_sme:
**	mrs	x([0-9]+), svcr
**	and	w0, w\1, #?1
**	ret
*/
PROTO (test_sme, int, ()) { return __arm_in_streaming_mode (); }
