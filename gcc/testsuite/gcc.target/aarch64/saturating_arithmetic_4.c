/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** uadd:
**	adds\tx([0-9]+), x([0-9]+), x([0-9]+)
**	csinv\tx\1, x\1, xzr, cc
**	ret
*/
/*
** uadd2:
**	adds\tx([0-9]+), x([0-9]+), x([0-9]+)
**	csinv\tx\1, x\1, xzr, cc
**	ret
*/
/*
** usub:
**	subs\tx([0-9]+), x([0-9]+), x([0-9]+)
**	csel\tx\1, x\1, xzr, cs
**	ret
*/

#include <limits.h>

#define UT unsigned long
#define UMAX ULONG_MAX
#define UMIN 0

#include "saturating_arithmetic.inc"