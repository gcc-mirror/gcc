/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
**	subs	(x[0-9]+), x0, #?2
**	csel	x0, \1, xzr, (cc|ls)
**	ret
*/
/*
** f2:
**	subs	(x[0-9]+), x0, #?2
**	csel	x0, \1, xzr, (cc|ls)
**	ret
*/
/*
** f3:
**	subs	(x[0-9]+), x0, #?1
**	csinc	x0, \1, xzr, ls
**	ret
*/
/*
** f4:
**	subs	(x[0-9]+), x0, #?3
**	csinv	x0, \1, xzr, cc
**	ret
*/

#define TYPE uint64_t
#define TYPE_MIN 0
#define TYPE_MAX UINT64_MAX
#define VALUE 2

#include "min_plus_1.c"
