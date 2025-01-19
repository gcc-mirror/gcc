/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** uadd:
**	adds\tw([0-9]+), w([0-9]+), w([0-9]+)
**	csinv\tw\1, w\1, wzr, cc
**	ret
*/
/*
** uadd2:
**	adds\tw([0-9]+), w([0-9]+), w([0-9]+)
**	csinv\tw\1, w\1, wzr, cc
**	ret
*/
/*
** usub:
**	subs\tw([0-9]+), w([0-9]+), w([0-9]+)
**	csel\tw\1, w\1, wzr, cs
**	ret
*/

#include <limits.h>

#define UT unsigned int
#define UMAX UINT_MAX
#define UMIN 0

#include "saturating_arithmetic.inc"