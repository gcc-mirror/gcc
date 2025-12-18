/* { dg-do run { target float16 } } */
/* { dg-require-effective-target s390_mvx } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** signbit_reg:
**	vlgvh	(%r[0-9]+),%v0,0
**	risbgn	%r2,\1,64-1,128\+63,48\+1
**	br	%r14
*/

/*
** signbit_mem:
**	llh	(%r[0-9]+),0\(%r2\)
**	risbgn	%r2,\1,64-1,128\+63,48\+1
**	br	%r14
*/

#include "float16-signbit.h"
