/* PR tree-optimization/56695 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

int a, b, i;

void
f (void)
{
  for (i = 0; i < 8; ++i)
    a |= !(i |= b %= 1);
}

/* { dg-final { cleanup-tree-dump "vect" } } */
