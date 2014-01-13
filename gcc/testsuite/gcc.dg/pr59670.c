/* PR middle-end/59670 */
/* { dg-do compile } */
/* { dg-options "-O1 -fopenmp-simd" } */

int d[1024];

int
foo (int j, int b)
{
  int l, c = 0;
#pragma omp simd reduction(+: c)
  for (l = 0; l < b; ++l)
    c += d[j + l];
  return c;
}
