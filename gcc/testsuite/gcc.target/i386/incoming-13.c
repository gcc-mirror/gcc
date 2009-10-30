/* PR target/40838 */
/* { dg-do compile { target { { ! *-*-darwin* } && ilp32 } } } */
/* { dg-options "-w -mstackrealign -O2 -mpreferred-stack-boundary=4" } */

extern double y(double *s3);

extern double s1, s2;

double x(void)
{
  double s3 = s1 + s2;
  return y(&s3);
}

/* { dg-final { scan-assembler-not "andl\[\\t \]*\\$-16,\[\\t \]*%esp" } } */
