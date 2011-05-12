/* PR tree-optimization/48975 */
/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -fno-tree-slp-vectorize" } */

static int
foo (int x)
{
  return (x > 0) ? 0 : x + 1;
}

void
bar (unsigned int x)
{
  int l = 1;
lab:
  while (x)
    x = foo (x);
}
