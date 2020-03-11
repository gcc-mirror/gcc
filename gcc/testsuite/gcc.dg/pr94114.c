/* PR tree-optimization/94114 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribute-patterns -ftrapv" } */

void
foo (int *x, int *y, int *z, long int w)
{
  while (y + w > z)
    {
      x[w] = 0;
      --w;
    }
}
