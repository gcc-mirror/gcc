/* PR tree-optimization/91201 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3 -mtune=k8 -masm=att" } */
/* { dg-final { scan-assembler-not "\tmovd\t%xmm0, %eax" } } */
/* { dg-final { scan-assembler "\tmov(zbl|b)\t\[^\n\r]*\\(%" } } */

typedef unsigned char V __attribute__((vector_size (16)));

unsigned char
foo (V x)
{
  return x[0];
}
