/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#define SHARED_ZT0
#include "test_sme2_acle.h"

/*
** str_zt0_x0:
**	str	zt0, \[x0\]
**	ret
*/
PROTO (str_zt0_x0, void, (char *x0)) { svstr_zt (0, x0); }

/*
** str_zt0_x0p1:
**	add	(x[0-9]+), x0, #?1
**	str	zt0, \[\1\]
**	ret
*/
PROTO (str_zt0_x0p1, void, (char *x0)) { svstr_zt (0, x0 + 1); }

/*
** str_zt0_x0p64:
**	add	(x[0-9]+), x0, #?64
**	str	zt0, \[\1\]
**	ret
*/
PROTO (str_zt0_x0p64, void, (char *x0)) { svstr_zt (0, x0 + 64); }

/*
** str_zt0_x0_vl1:
**	incb	x0
**	str	zt0, \[x0\]
**	ret
*/
PROTO (str_zt0_x0_vl1, void, (char *x0)) { svstr_zt (0, x0 + svcntb()); }
