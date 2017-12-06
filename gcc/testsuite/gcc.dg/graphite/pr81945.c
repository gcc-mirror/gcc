/* PR tree-optimization/81945 */
/* { dg-do compile { target pthread } } */
/* { dg-options "-O3 -ftree-parallelize-loops=2 -floop-nest-optimize" } */

unsigned long int v;

void
foo (int x, int y, long int *a)
{
  do
    {
      int **b;

      while (y != 0)
        ;
      v *= 2;
      **b = *a;
      ++x;
    }
  while (x < 1);
}
