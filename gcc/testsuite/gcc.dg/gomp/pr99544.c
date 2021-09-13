/* PR tree-optimization/99544 */
/* { dg-do compile } */
/* { dg-options "-Os -fopenmp" } */

long
foo (long a, long b, long c)
{
  long d, e;
  #pragma omp teams distribute parallel for simd firstprivate (a, b, c) lastprivate(e)
  for (d = a; d < b; d++)
    e = c + d * 5;
  return e;
}
