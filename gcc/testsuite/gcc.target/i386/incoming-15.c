/* PR target/40838 */
/* { dg-do compile { target { { ! *-*-darwin* } && ia32 } } } */
/* { dg-options "-w -mstackrealign -O2 -mpreferred-stack-boundary=4" } */

extern long long y(long long *s3);

extern long long s1, s2;

long long x(void)
{
  long long s3 = s1 + s2;
  return y(&s3);
}

/* { dg-final { scan-assembler-not "andl\[\\t \]*\\$-16,\[\\t \]*%esp" } } */
