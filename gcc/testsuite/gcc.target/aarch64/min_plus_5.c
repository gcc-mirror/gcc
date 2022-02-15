/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
**	subs	(w[0-9]+), w0, #?4095
**	csel	w0, \1, wzr, (cc|ls)
**	ret
*/
/*
** f2:
**	subs	(w[0-9]+), w0, #?4095
**	csel	w0, \1, wzr, (cc|ls)
**	ret
*/
/*
** f3:
**	subs	(w[0-9]+), w0, #?4094
**	csinc	w0, \1, wzr, ls
**	ret
*/
/*
** f4:
**	subs	(w[0-9]+), w0, #?4096
**	csinv	w0, \1, wzr, cc
**	ret
*/

#define TYPE uint32_t
#define TYPE_MIN 0
#define TYPE_MAX UINT32_MAX
#define VALUE 4095

#include "min_plus_1.c"
