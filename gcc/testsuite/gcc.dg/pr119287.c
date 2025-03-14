/* PR tree-optimization/119287 */
/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv" } */

unsigned a;
int b;
signed char c, d;

void
foo (void)
{
  c = a >> 14 & 1;
  for (; d;)
    c = 1;
  b = c << 14;
}
