/* PR tree-optimization/111845 */
/* { dg-do compile } */
/* { dg-options "-O2 --param tree-reassoc-width=2" } */

int a, b;
unsigned int c, d, e;

void
foo (int x)
{
  b += d;
  c += b < d;
  b += e = a < x;
  c += b;
  c += b < e;
}
