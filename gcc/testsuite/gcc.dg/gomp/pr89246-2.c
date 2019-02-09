/* PR middle-end/89246 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O0 -fno-openmp -fno-openmp-simd" } */

#pragma omp declare simd
extern int foo (__int128 x);

#pragma omp declare simd
int
bar (int x)
{
  return x + foo (0);
}
