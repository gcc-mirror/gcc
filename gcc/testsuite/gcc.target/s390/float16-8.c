/* { dg-do run { target float16 } } */
/* { dg-options "-O2 -march=z10 -save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** signbit_reg:
**	lgdr	(%r[0-9]+),%f0
**	srlg	(%r[0-9]+),\1,63
**	lgfr	%r2,\2
**	br	%r14
*/

/*
** signbit_mem:
**	lh	(%r[0-9]+),0\(%r2\)
**	llhr	(%r[0-9]+),\1
**	srl	\2,15
**	lgfr	%r2,\2
**	br	%r14
*/

#include "float16-signbit.h"
