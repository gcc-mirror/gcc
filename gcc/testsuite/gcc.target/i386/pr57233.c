/* PR tree-optimization/57233 */
/* { dg-do compile { target avx } } */
/* { dg-options "-O2 -mavx -mno-xop" } */

typedef unsigned V4 __attribute__((vector_size(4 * sizeof (int))));
V4 a;

__attribute__((noinline)) void
foo (void)
{
  a = (a << 2) | (a >> 30);
}

/* { dg-final { scan-assembler "vpsrld\[^\n\r]*30" } } */
/* { dg-final { scan-assembler "vpslld\[^\n\r]*2" } } */
