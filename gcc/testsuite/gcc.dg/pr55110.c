/* PR tree-optimization/55110 */
/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vectorize" } */

int
foo (int x)
{
  int a, b;
  for (b = 0; b < 8; b++)
    for (a = 0; a < 2; a++)
      x /= 3;
  return x;
}
