/* { dg-do compile } */
/* { dg-options "-O2" } */

/* A trapping comparison takes precedence over comparisons that are cheaper
   at the start of the sequence.  */

int
trap_before_zero (double a, double b, double c, double x, double y,
		  int i, int j)
{
  return (a == 0.0) & (x < y) & (c == 0.0) & (i < j);
}

/* { dg-final { scan-assembler-times {\tfcmpe\t} 1 } } */
/* { dg-final { scan-assembler-times {\tfccmp\t} 2 } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 1 } } */
/* { dg-final { scan-assembler-times {\tcset\t} 1 } } */
/* { dg-final { scan-assembler-not {\tand\t} } } */
