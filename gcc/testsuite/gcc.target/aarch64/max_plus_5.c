/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
**	adds	(w[0-9]+), w0, #4095
**	csel	w0, \1, wzr, (cs|hi)
**	ret
*/
/*
** f2:
**	adds	(w[0-9]+), w0, #4095
**	csel	w0, \1, wzr, (cs|hi)
**	ret
*/
/*
** f3:
**	adds	(w[0-9]+), w0, #4096
**	csinc	w0, \1, wzr, hi
**	ret
*/
/*
** f4:
**	adds	(w[0-9]+), w0, #4094
**	csinv	w0, \1, wzr, cs
**	ret
*/

#define TYPE uint32_t
#define TYPE_MIN 0
#define TYPE_MAX UINT32_MAX
#define VALUE (uint32_t)-4095

#include "max_plus_1.c"
