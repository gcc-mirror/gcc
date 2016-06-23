/* PR middle-end/68339 */
/* { dg-do compile } */
/* { dg-additional-options "--param ggc-min-heapsize=0 --param ggc-min-expand=0 -fopenmp-simd" } */

#pragma omp declare simd notinbranch
int
f1 (int x)
{
  return x;
}

#pragma omp declare simd notinbranch
int
f2 (int x)
{
  return x;
}
