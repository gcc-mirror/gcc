/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=neoverse-v2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* IRA moves constant moves between cmp and csel
   and never recovers since they use the same register
   #s. */

/*
** f1: { xfail *-*-* }
**	...
**	cmp	w[0-9]+, w[0-9]+
**	csel	w[0-9]+, w[0-9]+, w[0-9]+, (le|gt)
**	add	w[0-9]+, w[0-9]+, w[0-9]+
**	ret
*/
int f1 (int a, int b, int c)
{
  int cmp = a > b;
  int add1 = c + 3;
  int add2 = c + 8;
  return cmp ? add1 : add2;
}

/*
** f2: { xfail *-*-* }
**	...
**	cmp	x[0-9]+, x[0-9]+
**	csel	x[0-9]+, x[0-9]+, x[0-9]+, (le|gt)
**	add	x[0-9]+, x[0-9]+, x[0-9]+
**	ret
*/
long long f2 (long long a, long long b, long long c)
{
 long long cmp = a > b;
  long long add1 = c + 3;
  long long add2 = c + 8;
  return cmp ? add1 : add2;
}
