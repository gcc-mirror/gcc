/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest|\tor" { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not "\tnot" { xfail cc0 } } } */
/* { dg-final { scan-assembler-not "\tlsr" { xfail cc0 } } } */

int f(long long int a, long long int b, int *d)
{
  long long int c = a - b;

  *d = (c == 0LL);

  /* See pr93372-2.c; we have the same problem for DImode, but it's
     worsened by the generic double-word "optimizations"; or:ing
     together the DI parts and then testing the result for the equality
     test.  */
  return c >= 0LL;
}
