/* PR tree-optimization/87898 */
/* { dg-do compile { target fgraphite } } */
/* { dg-options "-O1 -floop-parallelize-all -fopenmp -ftree-parallelize-loops=2 -g" } */

#pragma omp declare simd
void
foo (int x)
{
  x = 0;
}
