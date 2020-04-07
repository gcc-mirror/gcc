/* PR target/93069 */
/* { dg-do assemble { target vect_simd_clones } } */
/* { dg-options "-O2 -fopenmp-simd -mtune=skylake-avx512" } */
/* { dg-additional-options "-mavx512vl" { target avx512vl } } */
/* { dg-additional-options "-mavx512dq" { target avx512dq } } */

#pragma omp declare simd
int
foo (int x, int y)
{
  return x == 0 ? x : y;
}
