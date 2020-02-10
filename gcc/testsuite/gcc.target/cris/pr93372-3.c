/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "\tcmp" 1 } } */

#ifndef t
#define t int
#endif

int ff(t a, t b, t *d)
{
  *d = (a == b);

  return a > b;
}
