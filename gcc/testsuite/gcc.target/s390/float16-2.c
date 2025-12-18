/* { dg-do compile { target float16 } } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target s390_mvx } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** fpr_to_gpr:
**     vlgvh	%r2,%v0,0
**     br	%r14
*/

/*
** gpr_to_fpr:
**     vlvgh	%v0,%r2,0
**     br	%r14
*/

/*
** load_into_fpr:
**     vleh	%v0,0\(%r2\),0
**     br	%r14
*/

/*
** load_into_gpr:
**     llgh	%r2,0\(%r2\)
**     br	%r14
*/

/*
** store:
**     vsteh	%v0,0\(%r2\),0
**     br	%r14
*/

#include "float16-1-2.h"
