/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not "\tnot" { xfail cc0 } } } */
/* { dg-final { scan-assembler-not "\tlsr" { xfail cc0 } } } */

int f(int a, int b, int *d)
{
  int c = a - b;

  /* Whoops!  We get a cmp.d with the original operands here. */
  *d = (c == 0);

  /* Whoops!  While we don't get a test.d for the result here for cc0,
     we get a sequence of insns: a move, a "not" and a shift of the
     subtraction-result, where a simple "spl" would have done. */
  return c >= 0;
}
