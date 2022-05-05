/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
**	adds	(w[0-9]+), w0, #4095
**	csel	w0, \1, wzr, g[te]
**	ret
*/
/*
** f2:
**	adds	(w[0-9]+), w0, #4095
**	csel	w0, \1, wzr, g[te]
**	ret
*/
/*
** f3:
**	adds	(w[0-9]+), w0, #4096
**	csinc	w0, \1, wzr, gt
**	ret
*/
/*
** f4:
**	adds	(w[0-9]+), w0, #4094
**	csinv	w0, \1, wzr, ge
**	ret
*/

#define TYPE int32_t
#define TYPE_MIN INT32_MIN
#define TYPE_MAX INT32_MAX
#define VALUE -4095

#include "max_plus_1.c"
