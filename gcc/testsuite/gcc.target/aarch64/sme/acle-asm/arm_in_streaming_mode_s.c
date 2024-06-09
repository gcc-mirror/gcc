/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define NO_SHARED_ZA
#include "test_sme_acle.h"

/*
** test_sme:
**	mov	w0, #?1
**	ret
*/
PROTO (test_sme, int, ()) { return __arm_in_streaming_mode (); }
