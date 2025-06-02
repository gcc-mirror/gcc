/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* PR target/120372 */

/* Go from 2 moves to none.  */

/*
** GT:
**	...
**	cmp	w0, 11182080
**	...
*/

int
GT (unsigned int x)
{
  return x > 0xaa9fff;
}

/*
** LE:
**	...
**	cmp	w0, 11182080
**	...
*/

int
LE (unsigned int x)
{
  return x <= 0xaa9fff;
}

/*
** GE:
**	...
**	cmp	x0, 11182080
**	...
*/

int
GE (long long x)
{
  return x >= 0xaaa000;
}

/*
** LT:
**	...
**	cmp	w0, 11182080
**	...
*/

int
LT (int x)
{
  return x < 0xaaa000;
}

/* Optimize the immediate in conditionals.  */

/*
** check:
**	...
**	cmp	w0, 11182080
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
**	cmp	w0, 11182080
**	...
*/

int
tern (int x)
{
  return x >= 0xaaa000 ? 5 : -3;
}
