/* { dg-do compile } */
/* { dg-options "-O2 -fsignaling-nans" } */

/* Every comparison raises Invalid for a signalling NaN, so none of them can
   become conditional.  */

int
and_eq (double a, double b, double c, double d)
{
  return (a == b) & (c == d);
}

/* { dg-final { scan-assembler-times {\tfcmp\td[0-9]+, d[0-9]+} 2 } } */
/* { dg-final { scan-assembler-not {\tfccmpe?\t} } } */
