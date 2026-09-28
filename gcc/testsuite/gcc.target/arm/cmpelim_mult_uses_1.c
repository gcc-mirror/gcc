/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f:
**	ands	r0, r0, #2097152
**	...
*/
void
f (unsigned x, int *p, int *q)
{
  int t = x & (1 << 21);
  *p = t ? 3 : 1;
  *q = t ? 2 : 0;
}

/*
** g:
**	adds	r0, r0, r1
**	...
*/
int
g (int x, int y, int *p, int *q)
{
  int t = x + y;
  *p = t ? 3 : 1;
  *q = t ? 5 : 0;
  return t;
}
