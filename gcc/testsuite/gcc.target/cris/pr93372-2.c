/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */
/* { dg-final { scan-assembler-not "\tnot" } } */
/* { dg-final { scan-assembler-not "\tlsr" } } */
/* We should get just one move, storing the result into *d.  */
/* { dg-final { scan-assembler-times "\tmove" 1 } } */

int f(int a, int b, int *d)
{
  int c = a - b;

  /* We used to get a cmp.d with the original operands here. */
  *d = (c == 0);

  /* We used to get a suboptimal sequence, but now we get the optimal "sge"
     (a.k.a "spl") re-using flags from the subtraction. */
  return c >= 0;
}
