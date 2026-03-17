/* { dg-options "-O2 -ftrapv" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

int sub(int a, int b)
{
  return a - b;
}

/*
** sub:
** ...
** (
**	bl	__subvsi3
** |
   Not generated yet, but would be equally acceptable.
**	subs	r0, r0, r1
**	bv[sc]	.*
** )
** ...
*/
