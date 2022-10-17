/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
**	subs	(x[0-9]+), x0, #?4094
**	csel	x0, \1, xzr, l[te]
**	ret
*/
/*
** f2:
**	subs	(x[0-9]+), x0, #?4094
**	csel	x0, \1, xzr, l[te]
**	ret
*/
/*
** f3:
**	subs	(x[0-9]+), x0, #?4093
**	csinc	x0, \1, xzr, le
**	ret
*/
/*
** f4:
**	subs	(x[0-9]+), x0, #?4095
**	csinv	x0, \1, xzr, lt
**	ret
*/

#define TYPE int64_t
#define TYPE_MIN INT64_MIN
#define TYPE_MAX INT64_MAX
#define VALUE 4094

#include "min_plus_1.c"
