/* PR middle-end/104757 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp" } */
// { dg-additional-options "-Wno-deprecated-openmp" }
#pragma omp declare target
void
foo (int x, int y, int *z)
{
  int j = 0;
  #pragma omp simd linear(j:x + y)
  for (int i = 0; i < 64; i++)
    j += x + y;
}
#pragma omp end declare target
