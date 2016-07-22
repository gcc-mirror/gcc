/* PR tree-optimization/56695 */
/* { dg-do compile } */

int a, b, i;

void
f (void)
{
  for (i = 0; i < 8; ++i)
    a |= !(i |= b %= 1);
}

