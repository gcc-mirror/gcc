/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=neoverse-v2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** f1:
**	...
**	cmp	w[0-9]+, w[0-9]+
**	csel	w[0-9]+, w[0-9]+, w[0-9]+, (le|gt)
**	add	w[0-9]+, w[0-9]+, w[0-9]+
**	ret
*/
int f1 (int a, int b, int c, int d, int e)
{
  int cmp = a > b;
  int add1 = c + d;
  int add2 = c + e;
  return cmp ? add1 : add2;
}

/*
** f2:
**	...
**	cmp	x[0-9]+, x[0-9]+
**	csel	x[0-9]+, x[0-9]+, x[0-9]+, (le|gt)
**	add	x[0-9]+, x[0-9]+, x[0-9]+
**	ret
*/
long long f2 (long long a, long long b, long long c, long long d, long long e)
{
 long long cmp = a > b;
  long long add1 = c + d;
  long long add2 = c + e;
  return cmp ? add1 : add2;
}
