/* Test the __chkfeat ACLE intrinsic.  */
/* { dg-do compile } */
/* { dg-options "-O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_acle.h>

/*
** test_chkfeat:
** ...
**	mov	x16, 1
**	hint	40 // chkfeat x16
**	eor	x0, x16, 1
**	ret
*/
uint64_t
test_chkfeat ()
{
  return __chkfeat (1);
}
