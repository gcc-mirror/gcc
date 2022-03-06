/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
**	adds	(x[0-9]+), x0, #3
**	csel	x0, \1, xzr, (cs|hi)
**	ret
*/
/*
** f2:
**	adds	(x[0-9]+), x0, #3
**	csel	x0, \1, xzr, (cs|hi)
**	ret
*/
/*
** f3:
**	adds	(x[0-9]+), x0, #4
**	csinc	x0, \1, xzr, hi
**	ret
*/
/*
** f4:
**	adds	(x[0-9]+), x0, #2
**	csinv	x0, \1, xzr, cs
**	ret
*/

#define TYPE uint64_t
#define TYPE_MIN 0
#define TYPE_MAX UINT64_MAX
#define VALUE (uint64_t)-3

#include "max_plus_1.c"
