/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
** (
**	cmp	w0, w1
**	cset	w0, gt
** |
**	cmp	w1, w0
**	cset	w0, lt
** )
**	ret
*/
int
f1 (int x, int y)
{
  return (x < y) < (y < x);
}

/*
** f2:
**	mov	w0, 0
**	ret
*/
int
f2 (int x, int y)
{
  return (x <= y) < (x < y);
}

/*
** f3:
**	cmp	(w0, w1|w1, w0)
**	cset	w0, ne
**	ret
*/
int
f3 (int x, int y)
{
  return (x <= y) == (x < y);
}

/*
** f4:
**	cmp	(w0, w1|w1, w0)
**	cset	w0, ne
**	ret
*/
int
f4 (int x, int y)
{
  return (y < x) != (x < y);
}

/*
** f5:
**	cmp	(w0, w1|w1, w0)
**	cset	w0, eq
**	ret
*/
int
f5 (int x, int y)
{
  return (y >= x) > (y > x);
}

/*
** f6:
** (
**	cmp	w0, w1
**	cset	w0, ge
** |
**	cmp	w1, w0
**	cset	w0, le
** )
**	ret
*/
int
f6 (int x, int y)
{
  return (x < y) < (y <= x);
}

/*
** f7:
**	mov	w0, 1
**	ret
*/
int
f7 (int x, int y)
{
  return (x < y) <= (x <= y);
}

/*
** f8:
** (
**	cmp	w0, w1
**	cset	w0, hi
** |
**	cmp	w1, w0
**	cset	w0, (lo|cc)
** )
**	ret
*/
int
f8 (unsigned int x, unsigned int y)
{
  return (x < y) < (y < x);
}

/*
** f9:
** (
**	cmp	w0, w1
**	cset	w0, (hs|cs)
** |
**	cmp	w1, w0
**	cset	w0, ls
** )
**	ret
*/
int
f9 (unsigned int x, unsigned int y)
{
  return (x < y) < (y <= x);
}
