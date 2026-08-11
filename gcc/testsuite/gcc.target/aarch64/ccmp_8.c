/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Only the leading comparison of a conditional compare sequence is a plain
   comparison, which accepts constants that CCMP rejects.  */

/*
** int_head:
**	cmp	w4, 100
**	ccmp	w0, w1, 0, eq
**	ccmp	w2, w3, 0, lt
**	cset	w0, lt
**	ret
*/
int
int_head (int a, int b, int c, int d, int e)
{
  return (e == 100) & (a < b) & (c < d);
}

/* Both comparisons against a constant reach the leading pair, so the
   expander can compare their costs and lead with the constant that CCMP
   cannot encode.  */

/*
** int_two_constants:
**	cmp	w0, 100
**	ccmp	w1, w2, 0, eq
**	ccmp	w3, 3, 0, lt
**	cset	w0, eq
**	ret
*/
int
int_two_constants (int a, int b, int c, int d)
{
  return (a == 100) & (b < c) & (d == 3);
}
