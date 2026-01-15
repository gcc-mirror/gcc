/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_acle.h>

void
test (void *a)
{
  __pldx (1, 1, 1, a);
}
/*
** test:
**...
**	prfm\tPSTL2STRM, \[x[0-9]+\]
**...
**	ret
*/
