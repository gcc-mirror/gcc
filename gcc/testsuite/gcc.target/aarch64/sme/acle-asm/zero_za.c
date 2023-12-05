/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#include "test_sme_acle.h"

/*
** zero_za:
**	zero	{ za }
**	ret
*/
PROTO (zero_za, void, ()) { svzero_za (); }
