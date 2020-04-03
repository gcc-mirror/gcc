/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* See pr93372-5.c regarding the xfails.  */
/* { dg-final { scan-assembler-not "\tcmp|\ttest|\tor" { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not "\tnot" { xfail cc0 } } } */
/* { dg-final { scan-assembler-not "\tlsr" { xfail cc0 } } } */

int f(long long int a, long long int b, int *d)
{
  long long int c = a + b;

  *d = (c == 0);

  return c >= 0;
}
