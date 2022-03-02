/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
**	adds	(x[0-9]+), x0, #4096
**	csel	x0, \1, xzr, g[te]
**	ret
*/
/*
** f2:
**	adds	(x[0-9]+), x0, #4096
**	csel	x0, \1, xzr, g[te]
**	ret
*/
/* f3 out of range */
/*
** f4:
**	adds	(x[0-9]+), x0, #4095
**	csinv	x0, \1, xzr, ge
**	ret
*/

#define TYPE int64_t
#define TYPE_MIN INT64_MIN
#define TYPE_MAX INT64_MAX
#define VALUE -4096

#include "max_plus_1.c"
