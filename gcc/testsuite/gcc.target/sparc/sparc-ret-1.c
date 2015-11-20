/* { dg-do compile } */
/* { dg-skip-if "no register windows" { *-*-* } { "-mflat" } { "" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-mcpu=ultrasparc -O" } */

/* Make sure that Ultrasparc return insn do not read below the stack.  */

void toto (int *);

int bar (int a, int b, int c, int d, int e, int f, int g, int h)
{
  int res;

  toto (&res);
  return h;
}
/* { dg-final { scan-assembler "return\[ \t\]*%i7\\+8\n\[^\n\]*ld\[ \t\]*\\\[%sp\\+96\\\]" } } */

int bar2 ()
{
  int res;

  toto (&res);
  return res;
}
/* { dg-final { scan-assembler "return\[ \t\]*%i7\\+8\n\[^\n\]*nop" } } */
