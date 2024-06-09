/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#define SHARED_ZT0
#include "test_sme2_acle.h"

/*
** zero_zt0:
**	zero	{ zt0 }
**	ret
*/
PROTO (zero_zt0, void, ()) { svzero_zt (0); }
