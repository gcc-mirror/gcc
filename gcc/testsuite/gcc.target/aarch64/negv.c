/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

_Bool negvsi (int x)
{
  int result;
  return __builtin_sub_overflow (0, x, &result);
}
/*
** negvsi:
**	negs	wzr, w0
**	cset	w0, vs
**	ret
*/

_Bool negvdi (long x)
{
  long result;
  return __builtin_sub_overflow (0, x, &result);
}
/*
** negvdi:
**	negs	xzr, x0
**	cset	w0, vs
**	ret
*/
