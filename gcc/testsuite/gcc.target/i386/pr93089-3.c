/* PR target/93089 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp-simd -mtune=skylake-avx512" } */
/* { dg-final { scan-assembler "vmulps\[^\n\r]*zmm" } } */
/* { dg-final { scan-assembler "vmulps\[^\n\r]*ymm" } } */

#pragma omp declare simd notinbranch
float
foo (float x, float y)
{
  return x * y;
}
