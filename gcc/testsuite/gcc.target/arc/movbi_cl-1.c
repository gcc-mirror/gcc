/* { dg-do compile } */
/* { dg-options "-mcpu=nps400 -O2 -mbitops" } */

int
f (int i)
{
  return 0x6e00;
}
/* { dg-final { scan-assembler "mov(bi|l)\.cl" } } */
