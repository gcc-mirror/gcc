/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f:
**	ands	w0, w0, w1
**	...
*/
int
f (int x, int y, int a, int b, int c, int d, int *p)
{
  int t = x & y;
  p[0] = t ? a : b;
  p[1] = t ? b : c;
  p[2] = t ? c : d;
  p[3] = t ? d : a;
  return t;
}

/*
** g:
**	subs	x0, x0, x1
**	...
*/
long long
g (long long x, long long y, long long a, long long b, long long c,
   long long d, long long *p)
{
  long long t = x - y;
  p[0] = t ? a : b;
  p[1] = t ? b : c;
  p[2] = t ? c : d;
  p[3] = t ? d : a;
  return t;
}
