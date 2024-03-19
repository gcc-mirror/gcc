/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#define SHARED_ZT0
#include "test_sme2_acle.h"

/*
** ldr_zt0_x0:
**	ldr	zt0, \[x0\]
**	ret
*/
PROTO (ldr_zt0_x0, void, (char *x0)) { svldr_zt (0, x0); }

/*
** ldr_zt0_x0p1:
**	add	(x[0-9]+), x0, #?1
**	ldr	zt0, \[\1\]
**	ret
*/
PROTO (ldr_zt0_x0p1, void, (char *x0)) { svldr_zt (0, x0 + 1); }

/*
** ldr_zt0_x0p64:
**	add	(x[0-9]+), x0, #?64
**	ldr	zt0, \[\1\]
**	ret
*/
PROTO (ldr_zt0_x0p64, void, (char *x0)) { svldr_zt (0, x0 + 64); }

/*
** ldr_zt0_x0_vl1:
**	incb	x0
**	ldr	zt0, \[x0\]
**	ret
*/
PROTO (ldr_zt0_x0_vl1, void, (char *x0)) { svldr_zt (0, x0 + svcntb()); }
