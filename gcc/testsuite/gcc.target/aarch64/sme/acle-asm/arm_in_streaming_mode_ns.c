/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define NON_STREAMING
#include "test_sme_acle.h"

/*
** test_sme:
**	mov	w0, #?0
**	ret
*/
PROTO (test_sme, int, ()) { return __arm_in_streaming_mode (); }
