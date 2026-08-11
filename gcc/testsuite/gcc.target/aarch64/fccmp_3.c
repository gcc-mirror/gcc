/* { dg-do compile } */
/* { dg-options "-O2 -ffinite-math-only" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* One condition code mode lets GCC reuse a compare of the same operands.  */

/*
** cse:
**	fcmp	d0, d1
**	cset	w0, eq
**	cinc	w0, w0, mi
**	ret
*/
int
cse (double a, double b)
{
  return (a < b) + (a == b);
}
