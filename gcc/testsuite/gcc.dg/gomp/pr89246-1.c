/* PR middle-end/89246 */
/* { dg-do link { target { int128 && vect_simd_clones } } } */
/* { dg-options "-O2 -fopenmp-simd -w" } */
/* { dg-additional-sources "pr89246-2.c" } */

#pragma omp declare simd
int foo (__int128 x)
{
  return x;
}
/* { dg-warning "unsupported argument type ‘__int128’ for ‘simd’ functions" "" { target aarch64*-*-* } .-4 } */

#pragma omp declare simd
extern int bar (int x);

int
main ()
{
  return foo (0) + bar (0);
}
