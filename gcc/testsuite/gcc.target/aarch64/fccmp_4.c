/* { dg-do compile } */
/* { dg-options "-O2 -ffinite-math-only" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* FCMP compares against zero, FCCMP does not.  A comparison against a
   constant therefore has to reach the head of the sequence.  */

/*
** fp_zero_head:
**	fcmp	d4, #0\.0
**	fccmp	d0, d1, 0, mi
**	fccmp	d2, d3, 0, mi
**	cset	w0, mi
**	ret
*/
int
fp_zero_head (double a, double b, double c, double d, double e)
{
  return (e < 0.0) & (a < b) & (c < d);
}
