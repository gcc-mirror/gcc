/* PR tree-optimization/91201 */
/* { dg-do compile } */
/* { dg-options "-Os -msse2 -mno-sse3 -mtune=generic -masm=att" } */
/* { dg-final { scan-assembler "\tmovd\t%xmm0, %eax" } } */
/* { dg-final { scan-assembler-not "\\(%" } } */

typedef unsigned char V __attribute__((vector_size (16)));

unsigned char
foo (V x)
{
  return x[0];
}
