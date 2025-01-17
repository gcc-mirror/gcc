/* { dg-do-compile } */
/* { dg-options "-O2 --save-temps -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** uadd:
**	dup	v([0-9]+).8b, w1
**	dup	v([0-9]+).8b, w0
**	uqadd	b([0-9]+), (?:b\2, b\1|b\1, b\2)
**	umov	w0, v\3.b\[0\]
**	ret
*/
/*
** uadd2:
**	dup	v([0-9]+).8b, w1
**	dup	v([0-9]+).8b, w0
**	uqadd	b([0-9]+), (?:b\2, b\1|b\1, b\2)
**	umov	w0, v\3.b\[0\]
**	ret
*/
/*
** usub: { xfail *-*-* }
**	dup	v([0-9]+).8b, w1
**	dup	v([0-9]+).8b, w0
**	uqsub	b([0-9]+), b\1, b\2
**	umov	w0, v\3.b\[0\]
**	ret
*/

#include <limits.h>

#define UT unsigned char
#define UMAX UCHAR_MAX
#define UMIN 0

#include "saturating_arithmetic.inc"