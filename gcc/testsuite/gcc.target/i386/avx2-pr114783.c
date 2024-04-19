/* PR target/114783 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -mno-avx512f -masm=att" } */
/* { dg-final { scan-assembler "vpcmpeqd\[ \\t\]+%ymm\[01\], %ymm\[01\], %ymm0" } } */

typedef int V __attribute__((vector_size (32)));

V
foo (V x, V y)
{
  return x == y;
}
