/* PR tree-optimization/104675 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void baz (int i);

void
bar (_Complex int c, short s)
{
  c -= s;
  baz (__real__ c + __imag__ c);
}

void
foo (void)
{
  bar (-1 - 1i, 0);
}
