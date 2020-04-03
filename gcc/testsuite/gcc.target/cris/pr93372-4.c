/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */

int f(int a, int *b)
{
  /* As seen in powisf2, the result of a shift is checked for zeroness. */
  int c = a >> 1;
  *b = (c == 0);
  return c;
}
