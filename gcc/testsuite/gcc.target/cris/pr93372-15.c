/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "\tcmp|\ttest" 1 } } */

int f(int a, int b, int *d)
{
  int c = a + b;

  *d = (c == 0);

  /* See also pr93372-6.c.  We should get exactly one compare
     instruction for this condition. */
  return c <= 0;
}
