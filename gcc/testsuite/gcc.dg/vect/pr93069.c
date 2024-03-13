/* PR target/93069 */
/* { dg-do assemble { target vect_simd_clones } } */
/* { dg-additional-options "-O2 -fopenmp-simd" } */

#pragma omp declare simd
int
foo (int x, int y)
{
  return x == 0 ? x : y;
}
