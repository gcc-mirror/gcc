/* PR tree-optimization/45241 */
/* { dg-do compile } */
/* { dg-options "-fno-tree-vectorize" } */

int
foo (short x)
{
  short i, y;
  int sum;

  for (i = 0; i < x; i++)
    y = x * i;

  for (i = x; i > 0; i--)
    sum += y;

  return sum;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
