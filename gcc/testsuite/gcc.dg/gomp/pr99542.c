/* PR middle-end/89246 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O0 -fopenmp-simd" } */

#pragma omp declare simd
extern int foo (__int128 x);
/* { dg-warning "unsupported argument type '__int128' for simd" "" { target i?86-*-* x86_64-*-* aarch64*-*-* } .-1 } */

#pragma omp declare simd uniform (x)
extern int baz (__int128 x);

#pragma omp declare simd
int
bar (int x)
{
  return x + foo (0) + baz (0);
}
