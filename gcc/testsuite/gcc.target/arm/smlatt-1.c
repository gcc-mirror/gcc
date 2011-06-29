/* { dg-do compile } */
/* { dg-options "-O2 -march=armv7-a" } */

int
foo (int x, int in1, int in2)
{
  short a = (in1 & 0xffff0000) >> 16;
  short b = (in2 & 0xffff0000) >> 16;

  return x + b * a;
}

/* { dg-final { scan-assembler "smlatt" } } */
