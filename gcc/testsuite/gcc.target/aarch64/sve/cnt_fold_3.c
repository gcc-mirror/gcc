/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** f1:
**	sub	x0, x1, x0
**	incb	x0
**	ret
*/
uint64_t
f1 (int *ptr1, int *ptr2)
{
  return ((ptr2 - ptr1) + svcntw ()) * 4;
}

/*
** f2:
**	sub	x0, x1, x0
**	incb	x0, all, mul #4
**	ret
*/
uint64_t
f2 (int *ptr1, int *ptr2)
{
  return ((ptr2 - ptr1) + svcntb ()) * 4;
}

/*
** f3:
**	sub	x0, x1, x0
**	ret
*/
uint64_t
f3 (int *ptr1, int *ptr2)
{
  return (((int *) ((char *) ptr2 + svcntb ()) - ptr1) - svcntw ()) * 4;
}
