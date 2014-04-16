/* PR tree-optimization/60844 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */
/* { dg-additional-options "-mtune=atom" { target { i?86-*-* x86_64-*-* } } } */

void
foo (int *x, int y, int z)
{
  int b, c = x[0], d = x[1];
  for (b = 0; b < 1; b++)
    {
      int e = (y ? 1 : 0) | (d ? 2 : 0) | (z ? 1 : 0);
      e |= (c ? 2 : 0) | ((1 >> b) ? 1 : 0);
      x[2 + b] = e;
    }
}
