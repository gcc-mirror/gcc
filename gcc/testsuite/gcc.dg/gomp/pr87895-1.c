/* PR tree-optimization/87895 */
/* { dg-do compile } */
/* { dg-additional-options "-O0" } */

#pragma omp declare simd
int
foo (int x)
{
  if (x == 0)
    return 0;
}

#pragma omp declare simd
int
bar (int *x, int y)
{
  if ((y == 0) ? (*x = 0) : *x)
    return 0;
}
