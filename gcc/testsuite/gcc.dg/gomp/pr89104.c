/* PR c++/66676 */
/* PR ipa/89104 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp-simd" } */

#pragma omp declare simd uniform (x) aligned (x)
int
foo (int *x, int y)
{
  return x[y];
}
