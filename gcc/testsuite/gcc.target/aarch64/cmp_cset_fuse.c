/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=neoverse-v2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** f1:
**	cmp	w[0-9]+, w[0-9]+
**	cset	w[0-9]+, gt
**	...
*/
int g;
int f1 (int a, int b)
{
  int cmp = a > b;
  g = cmp + 1;
  return cmp;
}

/*
** f2:
**	cmp	x[0-9]+, x[0-9]+
**	cset	x[0-9]+, gt
**	...
*/
long long h;
long long f2 (long long a, long long b)
{
  long long cmp = a > b;
  h = cmp + 1;
  return cmp;
}
