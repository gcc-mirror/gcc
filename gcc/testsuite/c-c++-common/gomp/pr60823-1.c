/* PR tree-optimization/60823 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp-simd" } */

#ifdef __aarch64__
#pragma omp declare simd simdlen(2) notinbranch
#else
#pragma omp declare simd simdlen(4) notinbranch
#endif
int
foo (const double c1, const double c2)
{
  double z1 = c1, z2 = c2;
  int res = 100, i;

  for (i = 0; i < 100; i++)
    {
      res = (z1 * z1 + z2 * z2 > 4.0) ? (i < res ? i : res) : res;
      z1 = c1 + z1 * z1 - z2 * z2;
      z2 = c2 + 2.0 * z1 * z2;
    }
  return res;
}
