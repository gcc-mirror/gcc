/* { dg-do-compile } */
/* { dg-options "-O2 --save-temps -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** uadd:
**	dup	v([0-9]+).4h, w1
**	dup	v([0-9]+).4h, w0
**	uqadd	h([0-9]+), (?:h\2, h\1|h\1, h\2)
**	umov	w0, v\3.h\[0\]
**	ret
*/
/*
** uadd2:
**	dup	v([0-9]+).4h, w1
**	dup	v([0-9]+).4h, w0
**	uqadd	h([0-9]+), (?:h\2, h\1|h\1, h\2)
**	umov	w0, v\3.h\[0\]
**	ret
*/
/*
** usub: { xfail *-*-* }
**	dup	v([0-9]+).4h, w1
**	dup	v([0-9]+).4h, w0
**	uqsub	h([0-9]+), h\1, h\2
**	umov	w0, v\3.h\[0\]
**	ret
*/

#include <limits.h>

#define UT unsigned short
#define UMAX USHRT_MAX
#define UMIN 0

#include "saturating_arithmetic.inc"