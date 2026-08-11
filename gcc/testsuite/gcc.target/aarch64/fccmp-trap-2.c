/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* A bitwise operation evaluates both comparisons, so a conditional
   comparison would hide the Invalid exception that the second ordered
   comparison raises for a quiet NaN.  */

/*
** and_lt:
**	fcmpe	d0, d1
**	cset	w[0-9]+, mi
**	fcmpe	d2, d3
**	cset	w[0-9]+, mi
**	and	w0, w[0-9]+, w[0-9]+
**	ret
*/
int
and_lt (double a, double b, double c, double d)
{
  return (a < b) & (c < d);
}

/* An equality comparison only raises Invalid for a signalling NaN, which
   this test does not honour, so it can still be made conditional.  */

/*
** and_eq:
**	fcmp	d0, d1
**	fccmp	d2, d3, 0, eq
**	cset	w0, eq
**	ret
*/
int
and_eq (double a, double b, double c, double d)
{
  return (a == b) & (c == d);
}

/* Only the floating-point comparison has to stay unconditional.  The target
   rejects it in the conditional position, and the expander leads with it.  */

/*
** and_int:
**	fcmpe	d0, d1
**	ccmp	w0, w1, 0, mi
**	cset	w0, lt
**	ret
*/
int
and_int (double a, double b, int i, int j)
{
  return (a < b) & (i < j);
}
