/* { dg-do compile { target float16 } } */
/* { dg-options "-O2 -march=z10" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** fpr_to_gpr:
**     lgdr	(%r[0-9]+),%f0
**     srlg	(%r[0-9]+),\1,48
**     llghr	%r2,\2
**     br	%r14
*/

/*
** gpr_to_fpr:
**     sllg	(%r[0-9]+),%r2,48
**     ldgr	%f0,\1
**     br	%r14
*/

/*
** load_into_fpr:
**     lh	(%r[0-9]+),0\(%r2\)
**     sllg	(%r[0-9]+),\1,48
**     ldgr	%f0,\2
**     br	%r14
*/

/*
** load_into_gpr:
**     llgh	%r2,0\(%r2\)
**     br	%r14
*/

/*
** store:
**     lgdr	(%r[0-9]+),%f0
**     srlg	(%r[0-9]+),\1,48
**     sth	\2,0\(%r2\)
**     br	%r14
*/

#include "float16-1-2.h"
