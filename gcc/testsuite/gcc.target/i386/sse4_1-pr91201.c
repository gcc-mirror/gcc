/* PR tree-optimization/91201 */
/* { dg-do compile } */
/* { dg-options "-Os -msse4.1 -masm=att" } */
/* { dg-final { scan-assembler-not "\tmovzb(w|l)" } } */

typedef unsigned char V __attribute__((vector_size (16)));

unsigned short
foo (V x)
{
  return x[0];
}
