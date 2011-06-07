/* { dg-do compile } */
/* { dg-options "-O2 -march=armv7-a" } */

int
foo (int x, int in)
{
  short a = in & 0xffff;
  short b = (in & 0xffff0000) >> 16;

  return x + b * a;
}

/* { dg-final { scan-assembler "smlatb" } } */
