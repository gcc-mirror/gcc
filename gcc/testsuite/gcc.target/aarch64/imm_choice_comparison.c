/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Go from four moves to two.  */

/*
** foo:
**	...
**	mov	w[0-9]+, 2576980377
**	movk	x[0-9]+, 0x9999, lsl 32
**	...
*/

int
foo (long long x)
{
  return x <= 0x0000999999999998;
}

/*
** GT:
**	...
**	mov	w[0-9]+, -16777217
**	...
*/

int
GT (unsigned int x)
{
  return x > 0xfefffffe;
}

/*
** LE:
**	...
**	mov	w[0-9]+, -16777217
**	...
*/

int
LE (unsigned int x)
{
  return x <= 0xfefffffe;
}

/*
** GE:
**	...
**	mov	w[0-9]+, 4278190079
**	...
*/

int
GE (long long x)
{
  return x >= 0xff000000;
}

/*
** LT:
**	...
**	mov	w[0-9]+, -16777217
**	...
*/

int
LT (int x)
{
  return x < 0xff000000;
}

/* Optimize the immediate in conditionals.  */

/*
** check:
**	...
**	mov	w[0-9]+, -16777217
**	...
*/

int
check (int x, int y)
{
  if (x > y && GT (x))
    return 100;

  return x;
}

/*
** tern:
**	...
**	mov	w[0-9]+, -16777217
**	...
*/

int
tern (int x)
{
  return x >= 0xff000000 ? 5 : -3;
}
