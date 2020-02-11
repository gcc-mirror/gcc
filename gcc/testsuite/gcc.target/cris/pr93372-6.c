/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */
/* { dg-final { scan-assembler "\tlsr|\tsmi" } } */

/* Regarding the "lsr", see pr93372-2.c; we get a shift for the
   sign-bit.  For "<", that's equally optimal to smi; we just want this
   test to be different with the "<" instead of ">=".  */

int f(int a, int b, int *d)
{
  int c = a + b;

  *d = (c == 0);

  return c < 0;
}
