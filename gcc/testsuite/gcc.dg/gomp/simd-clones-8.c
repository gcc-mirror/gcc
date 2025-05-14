/* PR middle-end/115871 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fopenmp -w" } */

#pragma omp declare simd inbranch simdlen(32)
double
foo (double x)
{
  return x * 4.0;
}
